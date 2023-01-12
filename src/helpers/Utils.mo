import Int64 "mo:base/Int64";
import Nat64 "mo:base/Nat64";
import Nat "mo:base/Nat";
import Float "mo:base/Float";
import Array "mo:base/Array";
import List "mo:base/List";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Text "mo:base/Text";
import Char "mo:base/Char";
import Option "mo:base/Option";
import Prim "mo:prim";
import Int "mo:base/Int";
import Int32 "mo:base/Int32";
import Nat32 "mo:base/Nat32";
import JSON "JSON";
import Request "../treasury/models/Request";
import Proposal "../dao/models/Proposal";
import Vote "../dao/models/Vote";
import SHA "./SHA";
import Blob "mo:base/Blob";
import Time "mo:base/Time";
import Hex "./Hex";
import TrieMap "mo:base/TrieMap";
import Nat8 "mo:base/Nat8";
import SwapService "../services/SwapService";

module {

    private type JSON = JSON.JSON;
    private type Request = Request.Request;
    private type RequestDraft = Request.RequestDraft;
    private type Transfer = Request.Transfer;
    private type WithdrawLiquidity = Request.WithdrawLiquidity;
    private type Proposal = Proposal.Proposal;
    private type Member = Request.Member;
    private type Threshold = Request.Threshold;
    private type TransferDraft = Request.TransferDraft;
    private type MemberDraft = Request.MemberDraft;
    private type WithdrawLiquidityDraft = Request.WithdrawLiquidityDraft;
    private type ThresholdDraft = Request.ThresholdDraft;
    private type Vote = Vote.Vote;

    public func natToFloat(value:Nat): Float {
        return Float.fromInt(value)
    };

    public func floatToNat(value:Float): Nat {
        let int = Float.toInt(value);
        let text = Int.toText(int);
        return textToNat(text);
    };
    
    public func includesText(string: Text, term: Text): Bool {
        let stringArray = Iter.toArray<Char>(toLowerCase(string).chars());
        let termArray = Iter.toArray<Char>(toLowerCase(term).chars());

        var i = 0;
        var j = 0;

        while (i < stringArray.size() and j < termArray.size()) {
            if (stringArray[i] == termArray[j]) {
                i += 1;
                j += 1;
                if (j == termArray.size()) { return true; }
            } else {
                i += 1;
                j := 0;
            }
        };
        false
    };

    public func toLowerCase(value: Text) : Text {
        let chars = Text.toIter(value);
        var lower = "";
        for (c: Char in chars) {
        lower := Text.concat(lower, Char.toText(Prim.charToLower(c)));
        };
        return lower;
    };
    
    public func nat32ToInt(value: Nat32): Int {
        let int32 = Int32.fromNat32(value);
        Int32.toInt(int32);
    };


    public func textToNat32( txt : Text) : Nat32 {
        assert(txt.size() > 0);
        let chars = txt.chars();

        var num : Nat32 = 0;
        for (v in chars){
            let charToNum = Char.toNat32(v)-48;
            assert(charToNum >= 0 and charToNum <= 9);
            num := num * 10 +  charToNum;          
        };

        num;
    };

    public func textToNat( txt : Text) : Nat {
        assert(txt.size() > 0);
        let chars = txt.chars();

        var num : Nat = 0;
        for (v in chars){
            let charToNum = Char.toNat32(v)-48;
            assert(charToNum >= 0 and charToNum <= 9);
            num := num * 10 +  Nat32.toNat(charToNum);          
        };

        num;
    };

    public func requestToJson(request: Request): JSON {
        switch(request){
            case(#transfer(value)){
                _transferToJson(value);
            };
            case(#addMember(value)){
                _memberToJson(value)
            };
            case(#removeMember(value)){
                _memberToJson(value)
            };
            case(#threshold(value)){
                _thresholdToJson(value);
            };
            case(#swapFor(value)){
                _transferToJson(value);
            };
            case(#withdrawLiquidity(value)){
                _withdrawToJson(value);
            };
            case(#addLiquidity(value)){
                _transferToJson(value);
            };
        };
    };

    public func requestDraftToJson(request: RequestDraft): JSON {
        switch(request){
            case(#transfer(value)){
                _transferDraftToJson(value);
            };
            case(#addMember(value)){
                _memberDraftToJson(value)
            };
            case(#removeMember(value)){
                _memberDraftToJson(value)
            };
            case(#threshold(value)){
                _thresholdDraftToJson(value);
            };
            case(#swapFor(value)){
                _transferDraftToJson(value);
            };
            case(#withdrawLiquidity(value)){
                _withdrawDraftToJson(value);
            };
            case(#addLiquidity(value)){
                _transferDraftToJson(value);
            };
        };
    };

    private func _approvalToJson(value: TrieMap.TrieMap<Text, Nat>): [JSON] {
        var approvals:[JSON] = [];
        for((member, power) in value.entries()){
            let map : HashMap.HashMap<Text, JSON> = HashMap.HashMap<Text, JSON>(
                0,
                Text.equal,
                Text.hash,
            );
            map.put("member", #String(member));
            map.put("power", #Number(power));

            let json = #Object(map);
            approvals := Array.append(approvals,[json])
        };

        approvals;
        
    };

    private func _transferToJson(value: Transfer): JSON {
        var approvals:[JSON] = _approvalToJson(value.approvals);
        let map : HashMap.HashMap<Text, JSON> = HashMap.HashMap<Text, JSON>(
            0,
            Text.equal,
            Text.hash,
        );

        let executedAt = value.executedAt;
        switch(executedAt){
            case(?executedAt){
                map.put("executedAt", #Number(executedAt));
            };
            case(null) {

            };
        };

        switch(value.token){
            case(#icp){
                map.put("token", #String("ICP"));
            };
            case(#yc){
                map.put("token", #String("YC"));
            };
        };

        switch(value.error){
            case(?exist){
                map.put("error", #String(exist));
            };
            case(null) {
                map.put("error", #Null);
            };
        };

        map.put("id", #Number(Nat32.toNat(value.id)));
        map.put("proposalId", #Number(Nat32.toNat(value.proposalId)));
        map.put("amount", #Number(value.amount));
        map.put("recipient", #String(value.recipient));
        map.put("approvals", #Array(approvals));
        map.put("executed", #Boolean(value.executed));
        map.put("createdAt", #Number(value.createdAt));
        map.put("description", #String(_toHex(value.description)));

        #Object(map);
    };

    private func _withdrawToJson(value: WithdrawLiquidity): JSON {
        var approvals:[JSON] = _approvalToJson(value.approvals);
        let map : HashMap.HashMap<Text, JSON> = HashMap.HashMap<Text, JSON>(
            0,
            Text.equal,
            Text.hash,
        );

        let executedAt = value.executedAt;
        switch(executedAt){
            case(?executedAt){
                map.put("executedAt", #Number(executedAt));
            };
            case(null) {

            };
        };

        switch(value.error){
            case(?exist){
                map.put("error", #String(exist));
            };
            case(null) {
                map.put("error", #Null);
            };
        };

        map.put("id", #Number(Nat32.toNat(value.id)));
        map.put("proposalId", #Number(Nat32.toNat(value.proposalId)));
        map.put("amount", #Number(value.amount));
        map.put("approvals", #Array(approvals));
        map.put("executed", #Boolean(value.executed));
        map.put("createdAt", #Number(value.createdAt));
        map.put("description", #String(_toHex(value.description)));

        #Object(map);
    };

    private func _transferDraftToJson(value: TransferDraft): JSON {
        let map : HashMap.HashMap<Text, JSON> = HashMap.HashMap<Text, JSON>(
            0,
            Text.equal,
            Text.hash,
        );

        switch(value.token){
            case(#icp){
                map.put("token", #String("ICP"));
            };
            case(#yc){
                map.put("token", #String("YC"));
            };
        };

        map.put("amount", #Number(value.amount));
        map.put("recipient", #String(value.recipient));
        map.put("description", #String(_toHex(value.description)));

        #Object(map);
    };
    private func _withdrawDraftToJson(value: WithdrawLiquidityDraft): JSON {
        let map : HashMap.HashMap<Text, JSON> = HashMap.HashMap<Text, JSON>(
            0,
            Text.equal,
            Text.hash,
        );

        map.put("amount", #Number(value.amount));
        map.put("recipient", #String(value.recipient));
        map.put("description", #String(_toHex(value.description)));

        #Object(map);
    };

    public func _voteToJson(value: Vote): JSON {
        let map : HashMap.HashMap<Text, JSON> = HashMap.HashMap<Text, JSON>(
            0,
            Text.equal,
            Text.hash,
        );
        map.put("proposalId", #Number(Nat32.toNat(value.proposalId)));
        map.put("yay", #Boolean(value.yay));
        map.put("member", #String(value.member));
        map.put("power", #String(Float.toText(value.power)));
        map.put("timeStamp", #Number(value.timeStamp));

        #Object(map);
        
    };

    public func _proposalToJson(value: Proposal): JSON {
        let map : HashMap.HashMap<Text, JSON> = HashMap.HashMap<Text, JSON>(
            0,
            Text.equal,
            Text.hash,
        );

        switch(value){
            case(#upgrade(value)){
                let executedAt = value.executedAt;
                switch(executedAt){
                    case(?executedAt){
                        map.put("executedAt", #Number(executedAt));
                    };
                    case(null) {

                    };
                };

                switch(value.canister){
                    case(#dao){
                        map.put("canister", #String("Dao"));
                    };
                    case(#treasury) {
                        map.put("canister", #String("Treasury"));
                    };
                    case(#taxCollector) {
                        map.put("canister", #String("taxCollector"));
                    };
                    case(#swap) {
                        map.put("canister", #String("Swap"));
                    };
                };

                map.put("id", #Number(Nat32.toNat(value.id)));
                map.put("creator", #String(value.creator));
                map.put("title", #String(value.title));
                map.put("description", #String(_toHex(value.description)));
                map.put("source", #String(value.source));
                map.put("hash", #String(value.hash));
                map.put("yay", #String(Float.toText(value.yay)));
                map.put("nay", #String(Float.toText(value.nay)));
                map.put("executed", #Boolean(value.executed));
                map.put("timeStamp", #Number(value.timeStamp))
            };
            case(#treasury(value)){

                let executedAt = value.executedAt;
                switch(executedAt){
                    case(?executedAt){
                        map.put("executedAt", #Number(executedAt));
                    };
                    case(null) {

                    };
                };
                map.put("id", #Number(Nat32.toNat(value.id)));
                map.put("treasuryRequestId", #Number(Nat32.toNat(value.treasuryRequestId)));
                map.put("creator", #String(value.creator));
                map.put("title", #String(value.title));
                map.put("description", #String(_toHex(value.description)));
                map.put("vote", #Boolean(value.vote));
                map.put("yay", #String(Float.toText(value.yay)));
                map.put("nay", #String(Float.toText(value.nay)));
                map.put("executed", #Boolean(value.executed));
                map.put("timeStamp", #Number(value.timeStamp))
            };
            case(#tax(value)){
                let executedAt = value.executedAt;
                switch(executedAt){
                    case(?executedAt){
                        map.put("executedAt", #Number(executedAt));
                    };
                    case(null) {

                    };
                };

                switch(value.taxType){
                    case(#transaction(amount)){
                        map.put("type", #String("transaction"));
                        map.put("amount", #String(Float.toText(amount)));
                    };

                    case(#burn(amount)) {
                        map.put("type", #String("burn"));
                        map.put("amount", #String(Float.toText(amount)));
                    };

                    case(#reflection(amount)) {
                        map.put("type", #String("reflection"));
                        map.put("amount", #String(Float.toText(amount)));
                    };

                    case(#treasury(amount)) {
                        map.put("type", #String("treasury"));
                        map.put("amount", #String(Float.toText(amount)));
                    };

                    case(#marketing(amount)) {
                        map.put("type", #String("marketing"));
                        map.put("amount", #String(Float.toText(amount)));
                    };

                    case(#maxHolding(amount)) {
                        map.put("type", #String("maxHolding"));
                        map.put("amount", #String(Float.toText(amount)));
                    };
                };
                map.put("id", #Number(Nat32.toNat(value.id)));
                map.put("creator", #String(value.creator));
                map.put("title", #String(value.title));
                map.put("description", #String(_toHex(value.description)));
                map.put("yay", #Number(value.yay));
                map.put("nay", #Number(value.nay));
                map.put("executed", #Boolean(value.executed));
                map.put("timeStamp", #Number(value.timeStamp))
            };
            case(#treasuryAction(value)){
                let requestDraft = requestDraftToJson(value.request);
                let executedAt = value.executedAt;
                switch(executedAt){
                    case(?executedAt){
                        map.put("executedAt", #Number(executedAt));
                    };
                    case(null) {

                    };
                };
                map.put("id", #Number(Nat32.toNat(value.id)));
                map.put("creator", #String(value.creator));
                map.put("title", #String(value.title));
                map.put("description", #String(_toHex(value.description)));
                map.put("request", requestDraft);
                map.put("yay", #String(Float.toText(value.yay)));
                map.put("nay", #String(Float.toText(value.nay)));
                map.put("executed", #Boolean(value.executed));
                map.put("timeStamp", #Number(value.timeStamp))
            };
            case(#proposalCost(value)){
               
                let executedAt = value.executedAt;
                switch(executedAt){
                    case(?executedAt){
                        map.put("executedAt", #Number(executedAt));
                    };
                    case(null) {

                    };
                };
                map.put("id", #Number(Nat32.toNat(value.id)));
                map.put("creator", #String(value.creator));
                map.put("title", #String(value.title));
                map.put("description", #String(_toHex(value.description)));
                map.put("amount", #Number(value.amount));
                map.put("yay", #String(Float.toText(value.yay)));
                map.put("nay", #String(Float.toText(value.nay)));
                map.put("executed", #Boolean(value.executed));
                map.put("timeStamp", #Number(value.timeStamp))
            };
        };
        #Object(map);
    };

    private func _memberToJson(value: Member): JSON {
        var approvals:[JSON] = _approvalToJson(value.approvals);
        let map : HashMap.HashMap<Text, JSON> = HashMap.HashMap<Text, JSON>(
            0,
            Text.equal,
            Text.hash,
        );

        let executedAt = value.executedAt;
        switch(executedAt){
            case(?executedAt){
                map.put("executedAt", #Number(executedAt));
            };
            case(null) {

            };
        };

        switch(value.error){
            case(?exist){
                map.put("error", #String(exist));
            };
            case(null) {
                map.put("error", #Null);
            };
        };

        map.put("id", #Number(Nat32.toNat(value.id)));
        map.put("proposalId", #Number(Nat32.toNat(value.proposalId)));
        map.put("principal", #String(value.principal));
        map.put("power", #Number(value.power));
        map.put("description", #String(_toHex(value.description)));
        map.put("approvals", #Array(approvals));
        map.put("executed", #Boolean(value.executed));
        map.put("createdAt", #Number(value.createdAt));

        #Object(map);
    };

    private func _memberDraftToJson(value: MemberDraft): JSON {
        let map : HashMap.HashMap<Text, JSON> = HashMap.HashMap<Text, JSON>(
            0,
            Text.equal,
            Text.hash,
        );

        map.put("principal", #String(value.principal));
        map.put("power", #Number(value.power));
        map.put("description", #String(_toHex(value.description)));

        #Object(map);
    };

    private func _thresholdToJson(value: Threshold): JSON {
        var approvals:[JSON] = _approvalToJson(value.approvals);
        let map : HashMap.HashMap<Text, JSON> = HashMap.HashMap<Text, JSON>(
            0,
            Text.equal,
            Text.hash,
        );

        let executedAt = value.executedAt;
        switch(executedAt){
            case(?executedAt){
                map.put("executedAt", #Number(executedAt));
            };
            case(null) {

            };
        };

        switch(value.error){
            case(?exist){
                map.put("error", #String(exist));
            };
            case(null) {
                map.put("error", #Null);
            };
        };

        map.put("id", #Number(Nat32.toNat(value.id)));
        map.put("proposalId", #Number(Nat32.toNat(value.proposalId)));
        map.put("power", #Number(value.power));
        map.put("description", #String(_toHex(value.description)));
        map.put("approvals", #Array(approvals));
        map.put("executed", #Boolean(value.executed));
        map.put("createdAt", #Number(value.createdAt));

        #Object(map);
    };

    private func _thresholdDraftToJson(value: ThresholdDraft): JSON {
        let map : HashMap.HashMap<Text, JSON> = HashMap.HashMap<Text, JSON>(
            0,
            Text.equal,
            Text.hash,
        );
        map.put("power", #Number(value.power));
        map.put("description", #String(_toHex(value.description)));
        #Object(map);
    };

    public func swapTxReceiptToText(value: SwapService.TxReceipt): Text {
        switch(value){
            case(#Ok(value)){
                ""
            };
            case(#Err(value)){
                switch(value){
                    case(#InsufficientAllowance){
                        "InsufficientAllowance"
                    };
                    case(#InsufficientBalance){
                        "InsufficientBalance"
                    };
                    case(#InsufficientPoolBalance){
                        "InsufficientPoolBalance"
                    };
                    case(#ErrorOperationStyle){
                        "ErrorOperationStyle"
                    };
                    case(#Unauthorized){
                        "Unauthorized"
                    };
                    case(#LedgerTrap){
                        "LedgerTrap"
                    };
                    case(#ErrorTo){
                        "ErrorTo"
                    };
                    case(#Other(value)){
                        value
                    };
                    case(#BlockUsed){
                        "BlockUsed"
                    };
                    case(#AmountTooSmall){
                        "AmountTooSmall"
                    };
                    case(#Slippage(value)){
                        "Slippage: " #Nat.toText(value)
                    };
                }
            };
        }
    };

    public func updateRequest(request:Request,executed:Bool,error:?Text): Request {
        let now = Time.now();
        switch(request){
            case(#transfer(value)){
                let result = {
                    id = value.id;
                    proposalId = value.proposalId;
                    token = value.token;
                    amount = value.amount;
                    recipient = value.recipient;
                    approvals = value.approvals;
                    executed = executed;
                    createdAt = value.createdAt;
                    executedAt = ?now;
                    description = value.description;
                    error = error;
                };
                #transfer(result);
            };
            case(#addMember(value)){
                let result = {
                    id = value.id;
                    proposalId = value.proposalId;
                    principal = value.principal;
                    power = value.power;
                    description = value.description;
                    approvals = value.approvals;
                    executed = executed;
                    createdAt = value.createdAt;
                    executedAt = ?now;
                    error = error;
                };
                #addMember(result);
            };
            case(#removeMember(value)){
                let result = {
                    id = value.id;
                    proposalId = value.proposalId;
                    principal = value.principal;
                    power = value.power;
                    description = value.description;
                    approvals = value.approvals;
                    executed = executed;
                    createdAt = value.createdAt;
                    executedAt = ?now;
                    error = error;
                };
                #removeMember(result);
            };
            case(#threshold(value)){
                let result = {
                    id = value.id;
                    proposalId = value.proposalId;
                    power = value.power;
                    description = value.description;
                    approvals = value.approvals;
                    executed = executed;
                    createdAt = value.createdAt;
                    executedAt = ?now;
                    error = error;
                };
                #threshold(result);
            };
            case(#swapFor(value)){
                let result = {
                    id = value.id;
                    proposalId = value.proposalId;
                    token = value.token;
                    amount = value.amount;
                    recipient = value.recipient;
                    approvals = value.approvals;
                    executed = executed;
                    createdAt = value.createdAt;
                    executedAt = ?now;
                    description = value.description;
                    error = error;
                };
                #swapFor(result);
            };
            case(#withdrawLiquidity(value)){
                let result = {
                    id = value.id;
                    proposalId = value.proposalId;
                    amount = value.amount;
                    approvals = value.approvals;
                    executed = executed;
                    createdAt = value.createdAt;
                    executedAt = ?now;
                    description = value.description;
                    error = error;
                };
                #withdrawLiquidity(result);
            };
            case(#addLiquidity(value)){
                let result = {
                    id = value.id;
                    proposalId = value.proposalId;
                    token = value.token;
                    amount = value.amount;
                    recipient = value.recipient;
                    approvals = value.approvals;
                    executed = executed;
                    createdAt = value.createdAt;
                    executedAt = ?now;
                    description = value.description;
                    error = error;
                };
                #addLiquidity(result);
            };
        }
    };

    public func _toHex(value:Text): Text {
        Hex.encode(Blob.toArray(Text.encodeUtf8(value)));
    };

    public func hash(blob: Blob): Text {
        let sum256 = SHA.fromBlob(#sha256,blob);
        Hex.encode(Blob.toArray(sum256));
    };
}