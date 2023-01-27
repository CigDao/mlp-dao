import Prim "mo:prim";
import Iter "mo:base/Iter";
import Principal "mo:base/Principal";
import Float "mo:base/Float";
import Nat "mo:base/Nat";
import Int "mo:base/Int";
import Nat32 "mo:base/Nat32";
import Nat64 "mo:base/Nat64";
import Array "mo:base/Array";
import HashMap "mo:base/HashMap";
import TrieMap "mo:base/TrieMap";
import List "mo:base/List";
import Time "mo:base/Time";
import Text "mo:base/Text";
import Vote "./models/Vote";
import Proposal "./models/Proposal";
import Stake "./models/Stake";
import Http "../helpers/http";
import Utils "../helpers/Utils";
import JSON "../helpers/JSON";
import Constants "../Constants";
import Response "../models/Response";
import Cycles "mo:base/ExperimentalCycles";
import Result "mo:base/Result";
import Error "mo:base/Error";
import TokenService "../services/TokenService";
import TreasuryService "../services/TreasuryService";
import ControllerService "../services/ControllerService";
import TopUpService "../services/TopUpService";
import CansiterService "../services/CansiterService"

actor class Dao(token:Text, treasury:Text, topup:Text, proposalCost:Nat, stakedTime:Int) = this {

  private stable var _token = token;
  private stable var _topup = topup;
  private stable var _treasury = treasury;
  private stable var _stakedTime = stakedTime;
  private stable var proposalId:Nat32 = 1;
  private stable var voteId:Nat32 = 1;
  private stable var totalTokensStaked:Nat = 0;
  private stable var proposal:?Proposal = null;
  private var _proposalCost:Nat = proposalCost;

  private type ErrorMessage = { #message : Text;};
  private type Proposal = Proposal.Proposal;
  private type Stake = Stake.Stake;
  private type ProposalRequest = Proposal.ProposalRequest;
  private type Vote = Vote.Vote;
  private type JSON = JSON.JSON;
  private type ApiError = Response.ApiError;

  private stable var proposalVoteEntries : [(Nat32,[Vote])] = [];
  private var proposalVotes = HashMap.fromIter<Nat32,[Vote]>(proposalVoteEntries.vals(), 0, Nat32.equal, func (a : Nat32) : Nat32 {a});

  private stable var rejectedEntries : [(Nat32,Proposal)] = [];
  private var rejected = HashMap.fromIter<Nat32,Proposal>(rejectedEntries.vals(), 0, Nat32.equal, func (a : Nat32) : Nat32 {a});

  private stable var acceptedEntries : [(Nat32,Proposal)] = [];
  private var accepted = HashMap.fromIter<Nat32,Proposal>(acceptedEntries.vals(), 0, Nat32.equal, func (a : Nat32) : Nat32 {a});

  private stable var voteEntries : [(Nat32,Vote)] = [];
  private var votes = HashMap.fromIter<Nat32,Vote>(voteEntries.vals(), 0, Nat32.equal, func (a : Nat32) : Nat32 {a});

  private stable var stakeEntries : [(Principal,Stake)] = [];
  private var stake = HashMap.fromIter<Principal,Stake>(stakeEntries.vals(), 0, Principal.equal, Principal.hash);

  system func preupgrade() {
    proposalVoteEntries := Iter.toArray(proposalVotes.entries());
    voteEntries := Iter.toArray(votes.entries());
    rejectedEntries := Iter.toArray(rejected.entries());
    acceptedEntries := Iter.toArray(accepted.entries());
    stakeEntries := Iter.toArray(stake.entries());
  };

  system func postupgrade() {
    proposalVoteEntries := [];
    voteEntries := [];
    rejectedEntries := [];
    acceptedEntries := [];
    stakeEntries := [];
  };

  public query func getMemorySize(): async Nat {
      let size = Prim.rts_memory_size();
      size;
  };

  public query func getHeapSize(): async Nat {
      let size = Prim.rts_heap_size();
      size;
  };

  public query func getCycles(): async Nat {
      Cycles.balance();
  };

   public query func totalStaked(): async Nat {
    totalTokensStaked
   };

  public query func getStake(owner:Principal): async Stake {
    _getStake(owner)
  };

  public query func getStakedAmount(owner:Principal): async Nat {
    _getStakedAmount(owner)
  };


  public query func getProposal(): async ?Proposal {
      proposal;
  };

  public query func getProposalCost(): async Nat {
      _proposalCost;
  };

  public query func fetchAcceptedProposals(): async [Proposal] {
      _fetchAcceptedProposals();
  };

  public query func fetchRejectedProposals(): async [Proposal] {
      _fetchRejectedProposals();
  };

  private func _getMemorySize(): Nat {
      let size = Prim.rts_memory_size();
      size;
  };

  private func _getHeapSize(): Nat {
      let size = Prim.rts_heap_size();
      size;
  };

  private func _getCycles(): Nat {
      Cycles.balance();
  };

  private func _topUp(): async () {
    if (_getCycles() <= Constants.cyclesThreshold){
        await TopUpService.topUp();
    }
  };

  public shared({caller}) func stakeTokens(amount:Nat): async TokenService.TxReceipt {
    let staked = _getStake(caller);
    let daoCanister = Principal.fromActor(this);
    let allowance = await TokenService.allowance(caller, daoCanister, _token);
    if(allowance < amount){
      return #Err(#InsufficientAllowance);
    };
    let result = await TokenService.transferFrom(caller, daoCanister, amount, _token);
    switch(result){
      case(#Ok(value)){
        let _staked = {
          amount = staked.amount+amount;
          timeStamp = null
        };
        totalTokensStaked := totalTokensStaked + _staked.amount;
        stake.put(caller,_staked);
        return #Ok(value);
      };
      case(#Err(value)){
        #Err(value)
      }
    }
  };

  public shared({caller}) func startUnStaking(): async TokenService.TxReceipt {
    let now = Time.now();
    let staked = _getStake(caller);
    let timeStamp = staked.timeStamp;
    switch(timeStamp){
      case(?timeStamp){
        #Err(#Unauthorized);
      };  
      case(null){
        let _staked = {
          amount = staked.amount;
          timeStamp = ?now;
        };
        stake.put(caller,_staked);
        return #Ok(0);
      } 
    };
  };

  public shared({caller}) func unStakeTokens(amount:Nat): async TokenService.TxReceipt {
    let now = Time.now();
    let staked = _getStake(caller);
    let timeStamp = staked.timeStamp;
    assert(amount <= staked.amount);
    switch(timeStamp){
      case(?timeStamp){
        assert(timeStamp+stakedTime < now);
        let result = await TokenService.transfer(caller, amount, _token);
        switch(result){
          case(#Ok(value)){
            let _staked = {
              amount = staked.amount-amount;
              timeStamp = null
            };
            totalTokensStaked := totalTokensStaked - amount;
            stake.put(caller,_staked);
            return #Ok(value);
          };
          case(#Err(value)){
            #Err(value)
          }
        }
      };
      case(null){
        #Err(#Unauthorized);
      } 
    };
  };

  private func _getStake(owner:Principal) : Stake {
    let exist = stake.get(owner);
    switch(exist){
      case(?exist){
        exist
      };
      case(null){
        {
          amount = 0;
          timeStamp = null;
        }
      }
    };
  };

  private func _getStakedAmount(owner:Principal) : Nat {
    let exist = stake.get(owner);
    switch(exist){
      case(?exist){
        exist.amount
      };
      case(null){
        0
      }
    };
  };

  private func _canVote(owner:Principal) : Bool {
    let exist = _getStake(owner).timeStamp;
    switch(exist){
      case(?exist){
        false
      };
      case(null){
        true
      };
    }
  };

  public shared({caller}) func createProposal(request:ProposalRequest): async TokenService.TxReceipt {
    ignore _topUp();
    switch(proposal){
      case(?proposal){
        #Err(#ActiveProposal);
      };
      case(null){
        let result = await _createProposal(caller, request);
        switch(result){
          case(#Ok(value)){
            #Ok(value)
          };
          case(#Err(value)){
            #Err(value)
          };
        };
      }
    }
  };

  private func _createProposal(caller:Principal, request:ProposalRequest): async TokenService.TxReceipt {
    //verify the amount of tokens is approved
    ///ADD THIS BACK
    let allowance = await TokenService.allowance(caller,Principal.fromActor(this),_token);
    if(_proposalCost > allowance){
      return #Err(#InsufficientAllowance);
    };
    let result = await TokenService.transferFrom(caller,Principal.fromActor(this), _proposalCost, _token);
    switch(result){
      case(#Ok(value)){
        //verify hash if upgrading wasm
        switch(request){
          case(#upgrade(obj)){
            let hash = Utils.hash(obj.wasm);
            if(hash != obj.hash){
              return #Err(#Other("Invalid wasm. Wasm hash does not match source"));
            };
            //create proposal
            let currentId = proposalId;
            proposalId := proposalId+1;
            let upgrade = {
              id = currentId;
              creator = Principal.toText(caller);
              wasm = obj.wasm;
              args = obj.args;
              canister = obj.canister;
              title = obj.title;
              description = obj.description;
              source = obj.source;
              hash = obj.hash;
              yay = 0;
              nay = 0;
              executed = false;
              executedAt = null;
              timeStamp = Time.now();
            };
            proposal := ?#upgrade(upgrade);
            #Ok(Nat32.toNat(currentId));
          };
          case(#treasury(obj)){
            //create proposal
            let currentId = proposalId;
            proposalId := proposalId+1;
            let treasury = {
              id = currentId;
              treasuryRequestId = obj.treasuryRequestId;
              creator = Principal.toText(caller);
              vote = obj.vote;
              title = obj.title;
              description = obj.description;
              yay = 0;
              nay = 0;
              executed = false;
              executedAt = null;
              timeStamp = Time.now();
            };
            proposal := ?#treasury(treasury);
            #Ok(Nat32.toNat(currentId));
          };
          case(#treasuryAction(obj)){
            //create proposal
            let currentId = proposalId;
            proposalId := proposalId+1;
            let treasuryAction = {
              id = currentId;
              creator = Principal.toText(caller);
              request = obj.request;
              title = obj.title;
              description = obj.description;
              yay = 0;
              nay = 0;
              executed = false;
              executedAt = null;
              timeStamp = Time.now();
            };
            proposal := ?#treasuryAction(treasuryAction);
            #Ok(Nat32.toNat(currentId));
          };
          case(#proposalCost(obj)){
            //create proposal
            let currentId = proposalId;
            proposalId := proposalId+1;
            let proposalCost = {
              id = currentId;
              creator = Principal.toText(caller);
              amount = obj.amount;
              title = obj.title;
              description = obj.description;
              yay = 0;
              nay = 0;
              executed = false;
              executedAt = null;
              timeStamp = Time.now();
            };
            proposal := ?#proposalCost(proposalCost);
            #Ok(Nat32.toNat(currentId));
          }
        };
      };
      case(#Err(value)){
        return #Err(value)
      }
    };
  };

  public shared({caller}) func vote(proposalId:Nat32,yay:Bool): async TokenService.TxReceipt {
    ignore _topUp();
    let stakedTokens = _getStakedAmount(caller);
    assert(stakedTokens > 0);
    let canVote = _canVote(caller);
    assert(canVote);
    let isVoted = _voted(proposalId, caller);
    assert(isVoted == false);
    let vote = {
      proposalId = proposalId;
      yay = yay;
      member = Principal.toText(caller);
      power = stakedTokens;
      timeStamp = Time.now();
    };
    //credit vote
    let currentId = voteId;
    voteId := voteId+1;
    votes.put(voteId,vote);
    _addVoteToProposal(proposalId, vote);
    ignore _vote(proposalId, stakedTokens, yay);
    #Ok(Nat32.toNat(currentId));
  };

  private func _voted(id:Nat32, owner:Principal): Bool {
    let exist = proposalVotes.get(id);
    switch(exist){
      case(?exist){
        let member = Principal.toText(owner);
        let myVote = Array.find(exist,func(e:Vote):Bool{e.member == member});
        switch(myVote){
          case(?myVote){
            return true;
          };
          case(null){
            return false;
          };
        }
      };
      case(null){
        false;
      };
    };
  };

  private func _vote(proposalId:Nat32, power:Nat, yay:Bool): async () {
    let exist = proposal;
    switch(exist){
      case(?exist){
        switch(exist){
          case(#upgrade(value)){
            if(yay){
              var _proposal = {
                id = value.id;
                creator = value.creator;
                wasm = value.wasm;
                args = value.args;
                canister = value.canister;
                title = value.title;
                description = value.description;
                source = value.source;
                hash = value.hash;
                yay = value.yay + power;
                nay = value.nay;
                executed = value.executed;
                executedAt = value.executedAt;
                timeStamp = value.timeStamp;
              };
              proposal := ?#upgrade(_proposal);
            }else {
              var _proposal = {
                id = value.id;
                creator = value.creator;
                wasm = value.wasm;
                args = value.args;
                canister = value.canister;
                title = value.title;
                description = value.description;
                source = value.source;
                hash = value.hash;
                yay = value.yay;
                nay = value.nay + power;
                executed = value.executed;
                executedAt = value.executedAt;
                timeStamp = value.timeStamp;
              };
              proposal := ?#upgrade(_proposal);
            };
          };
          case(#treasury(value)){
            if(yay){
              var _proposal = {
                id = value.id;
                treasuryRequestId = value.treasuryRequestId;
                creator = value.creator;
                vote = value.vote;
                title = value.title;
                description = value.description;
                yay = value.yay + power;
                nay = value.nay;
                executed = value.executed;
                executedAt = value.executedAt;
                timeStamp = value.timeStamp;
              };
              proposal := ?#treasury(_proposal);
            }else {
              var _proposal = {
                id = value.id;
                treasuryRequestId = value.treasuryRequestId;
                creator = value.creator;
                vote = value.vote;
                title = value.title;
                description = value.description;
                yay = value.yay;
                nay = value.nay + power;
                executed = value.executed;
                executedAt = value.executedAt;
                timeStamp = value.timeStamp;
              };
              proposal := ?#treasury(_proposal);
            }
          };
          case(#treasuryAction(value)) {
            if(yay){
              var _proposal = {
                id = value.id;
                creator = value.creator;
                request = value.request;
                title = value.title;
                description = value.description;
                yay = value.yay + power;
                nay = value.nay;
                executed = value.executed;
                executedAt = value.executedAt;
                timeStamp = value.timeStamp;
              };
              proposal := ?#treasuryAction(_proposal);
            }else {
              var _proposal = {
                id = value.id;
                creator = value.creator;
                request = value.request;
                title = value.title;
                description = value.description;
                yay = value.yay;
                nay = value.nay + power;
                executed = value.executed;
                executedAt = value.executedAt;
                timeStamp = value.timeStamp;
              };
              proposal := ?#treasuryAction(_proposal);
            }
          };
          case(#proposalCost(value)) {
            if(yay){
              var _proposal = {
                id = value.id;
                creator = value.creator;
                amount = value.amount;
                title = value.title;
                description = value.description;
                yay = value.yay + power;
                nay = value.nay;
                executed = value.executed;
                executedAt = value.executedAt;
                timeStamp = value.timeStamp;
              };
              proposal := ?#proposalCost(_proposal);
            }else {
              var _proposal = {
                id = value.id;
                creator = value.creator;
                amount = value.amount;
                title = value.title;
                description = value.description;
                yay = value.yay;
                nay = value.nay + power;
                executed = value.executed;
                executedAt = value.executedAt;
                timeStamp = value.timeStamp;
              };
              proposal := ?#proposalCost(_proposal);
            }
          }
        };
      };
      case(null){

      };
    };
     ignore _tally();
  };

  private func _addVoteToProposal(proposalId:Nat32, vote:Vote) {
    let exist = proposalVotes.get(proposalId);
    switch(exist){
      case(?exist){
        let votes = Array.append(exist,[vote]);
        proposalVotes.put(proposalId,votes);
      };
      case(null){
        proposalVotes.put(proposalId,[vote]);
      }
    };
  };

  private func _tally(): async () {
    let _totalStaked = Utils.natToFloat(totalTokensStaked);
    let majority =  _totalStaked * 0.5;
    switch(proposal){
      case(?_proposal){
        switch(_proposal){
          case(#upgrade(value)){
            if(Utils.natToFloat(value.yay) > majority) {
              //accepted
              accepted.put(value.id,#upgrade(value));      
            }else if (Utils.natToFloat(value.nay) > majority) {
              rejected.put(value.id,#upgrade(value));
              //rejected
            }
          };
          case(#treasury(value)){
            if(Utils.natToFloat(value.yay) > majority) {
              //accepted
              var _proposal = {
                id = value.id;
                treasuryRequestId = value.treasuryRequestId;
                creator = value.creator;
                vote = value.vote;
                title = value.title;
                description = value.description;
                yay = value.yay;
                nay = value.nay;
                executed = true;
                executedAt = ?Time.now();
                timeStamp = value.timeStamp;
              };
              accepted.put(value.id,#treasury(_proposal));
              //make call to treasury cansiter that should be blackedhole
              //Add This Back
              ignore TreasuryService.approveRequest(value.treasuryRequestId, _treasury);
            }else if(Utils.natToFloat(value.nay) > majority){
              var _proposal = {
                id = value.id;
                treasuryRequestId = value.treasuryRequestId;
                creator = value.creator;
                vote = value.vote;
                title = value.title;
                description = value.description;
                yay = value.yay;
                nay = value.nay;
                executed = false;
                executedAt = ?Time.now();
                timeStamp = value.timeStamp;
              };
              rejected.put(value.id,#treasury(_proposal));
            }
          };
          case(#treasuryAction(value)) {
            if(Utils.natToFloat(value.yay) > majority) {
              //accepted
              var _proposal = {
                id = value.id;
                creator = value.creator;
                request = value.request;
                title = value.title;
                description = value.description;
                yay = value.yay;
                nay = value.nay;
                executed = true;
                executedAt = ?Time.now();
                timeStamp = value.timeStamp;
              };
              accepted.put(value.id,#treasuryAction(_proposal));
              //make call to treasury cansiter that should be blackedhole
              ignore TreasuryService.createRequest(value.id,value.request, _treasury);
            }else if(Utils.natToFloat(value.nay) > majority){
              var _proposal = {
                id = value.id;
                creator = value.creator;
                request = value.request;
                title = value.title;
                description = value.description;
                yay = value.yay;
                nay = value.nay;
                executed = false;
                executedAt = ?Time.now();
                timeStamp = value.timeStamp;
              };
              rejected.put(value.id,#treasuryAction(_proposal));
            }
          };
          case(#proposalCost(value)) {
            if(Utils.natToFloat(value.yay) > majority) {
              //accepted
              var _proposal = {
                id = value.id;
                creator = value.creator;
                amount = value.amount;
                title = value.title;
                description = value.description;
                yay = value.yay;
                nay = value.nay;
                executed = true;
                executedAt = ?Time.now();
                timeStamp = value.timeStamp;
              };
              accepted.put(value.id,#proposalCost(_proposal));
              _proposalCost := value.amount;
              proposal := null;
            }else if(Utils.natToFloat(value.nay) > majority){
              var _proposal = {
                id = value.id;
                creator = value.creator;
                amount = value.amount;
                title = value.title;
                description = value.description;
                yay = value.yay;
                nay = value.nay;
                executed = false;
                executedAt = ?Time.now();
                timeStamp = value.timeStamp;
              };
              rejected.put(value.id,#proposalCost(_proposal));
              proposal := null;
            }
          };
        };
      };
      case(null){

      }
    };
  };

  private func _upgradeController(wasm:Blob, arg:Blob, canisterId:Text): async () {
      let canisterId = Principal.fromText(Constants.controllerCanister);
      await CansiterService.CanisterUtils().installCode(canisterId, arg, wasm);
  };

  public query func http_request(request : Http.Request) : async Http.Response {
        let path = Iter.toArray(Text.tokens(request.url, #text("/")));

        if (path.size() == 1) {
            switch (path[0]) {
                case ("getProposal") return _proposalResponse();
                case ("proposalCost") return _natResponse(_proposalCost);
                case ("fetchAcceptedProposals") return _fetchAcceptedProposalResponse();
                case ("fetchRejectedProposals") return _fetchRejectedProposalResponse();
                case ("getMemorySize") return _natResponse(_getMemorySize());
                case ("getHeapSize") return _natResponse(_getHeapSize());
                case ("getCycles") return _natResponse(_getCycles());
                case (_) return return Http.BAD_REQUEST();
            };
        } else if (path.size() == 2) {
            switch (path[0]) {
                case ("fetchVotes") return _fetchVoteResponse(path[1]);
                case ("getVote") return _voteResponse(path[1]);
                case (_) return return Http.BAD_REQUEST();
            };
        }else if (path.size() == 3) {
            switch (path[0]) {
                case ("voted") return _votedResponse(path[1], path[2]);
                case (_) return return Http.BAD_REQUEST();
            };
        }else {
            return Http.BAD_REQUEST();
        };
    };

    private func _natResponse(value : Nat): Http.Response {
        let json = #Number(value);
        let blob = Text.encodeUtf8(JSON.show(json));
        let response: Http.Response = {
            status_code        = 200;
            headers            = [("Content-Type", "application/json")];
            body               = blob;
            streaming_strategy = null;
        };
    };

    private func _votedResponse(proposalId:Text, owner:Text) : Http.Response {
      let id = Utils.textToNat32(proposalId);
      let value = _voted(id, Principal.fromText(owner));
      let json = #Boolean(value);
      let blob = Text.encodeUtf8(JSON.show(json));
      let response: Http.Response = {
          status_code        = 200;
          headers            = [("Content-Type", "application/json")];
          body               = blob;
          streaming_strategy = null;
      };
    };

    private func _fetchAcceptedProposals(): [Proposal] {
      var results:[Proposal] = [];
      for ((id,request) in accepted.entries()) {
        results := Array.append(results,[request]);
      };
      results;
    };

    private func _fetchRejectedProposals(): [Proposal] {
      var results:[Proposal] = [];
      for ((id,request) in rejected.entries()) {
        results := Array.append(results,[request]);
      };
      results;
    };

    private func _fetchVotes(proposalId:Nat32): [Vote] {
      var results:[Vote] = [];
      let exist = proposalVotes.get(proposalId);
      switch(exist){
        case(?exist){
          exist;
        };
        case(null){
          [];
        }
      };
    };

    private func _fetchAcceptedProposalResponse() : Http.Response {
      let _proposals =  _fetchAcceptedProposals();
      var result:[JSON] = [];

      for(proposal in _proposals.vals()) {
        let json = Utils._proposalToJson(proposal);
        result := Array.append(result,[json]);
      };

      let json = #Array(result);
      let blob = Text.encodeUtf8(JSON.show(json));
      let response : Http.Response = {
          status_code = 200;
          headers = [("Content-Type", "application/json")];
          body = blob;
          streaming_strategy = null;
      };
    };

    private func _fetchRejectedProposalResponse() : Http.Response {
      let _proposals =  _fetchRejectedProposals();
      var result:[JSON] = [];

      for(proposal in _proposals.vals()) {
        let json = Utils._proposalToJson(proposal);
        result := Array.append(result,[json]);
      };

      let json = #Array(result);
      let blob = Text.encodeUtf8(JSON.show(json));
      let response : Http.Response = {
          status_code = 200;
          headers = [("Content-Type", "application/json")];
          body = blob;
          streaming_strategy = null;
      };
    };

    private func _fetchVoteResponse(value:Text) : Http.Response {
      let id = Utils.textToNat32(value);
      let _votes =  _fetchVotes(id);
      var result:[JSON] = [];

      for(obj in _votes.vals()) {
        let json = Utils._voteToJson(obj);
        result := Array.append(result,[json]);
      };

      let json = #Array(result);
      let blob = Text.encodeUtf8(JSON.show(json));
      let response : Http.Response = {
          status_code = 200;
          headers = [("Content-Type", "application/json")];
          body = blob;
          streaming_strategy = null;
      };
    };

    private func _proposalResponse() : Http.Response {
      let exist = proposal;
      switch(exist){
        case(?exist){
          let json = Utils._proposalToJson(exist);
          let blob = Text.encodeUtf8(JSON.show(json));
          let response : Http.Response = {
              status_code = 200;
              headers = [("Content-Type", "application/json")];
              body = blob;
              streaming_strategy = null;
          };
        };
        case(null){
          return Http.NOT_FOUND();
        };
      };
    };

    private func _voteResponse(value : Text) : Http.Response {
      let id = Utils.textToNat32(value);
      let exist = votes.get(id);
      switch(exist){
        case(?exist){
          let json = Utils._voteToJson(exist);
          let blob = Text.encodeUtf8(JSON.show(json));
          let response : Http.Response = {
              status_code = 200;
              headers = [("Content-Type", "application/json")];
              body = blob;
              streaming_strategy = null;
          };
        };
        case(null){
          return Http.NOT_FOUND();
        };
      };
    };

};