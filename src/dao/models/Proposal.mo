import Blob "mo:base/Blob";
import Time "mo:base/Time";
import Request "../../treasury/models/Request";

module {

    private type RequestDraft = Request.RequestDraft;

    public type Proposal = {
        #upgrade:Upgrade;
        #treasury:Treasury;
        #treasuryAction:TreasuryAction;
        #tax:Tax;
        #proposalCost:ProposalCost;
    };

    public type ProposalRequest = {
        #upgrade:UpgradeRequest;
        #treasury:TreasuryRequest;
        #treasuryAction:TreasuryActionRequest;
        #tax:TaxRequest;
        #proposalCost:ProposalCostRequest;
    };

    public type TaxType = {
        #transaction:Float;
        #burn:Float;
        #reflection:Float;
        #treasury:Float;
        #marketing:Float;
        #maxHolding:Float;
    };

    public type TaxRequest = {
        taxType:TaxType;
        title:Text;
        description:Text;
    };

    public type ProposalCostRequest = {
        amount:Nat;
        title:Text;
        description:Text;
    };

    public type Tax = {
        id:Nat32;
        creator:Text;
        taxType:TaxType;
        title:Text;
        description:Text;
        yay:Nat;
        nay:Nat;
        executed:Bool;
        executedAt:?Time.Time;
        timeStamp:Time.Time;
    };

    public type ProposalCost = {
        id:Nat32;
        creator:Text;
        amount:Nat;
        title:Text;
        description:Text;
        yay:Nat;
        nay:Nat;
        executed:Bool;
        executedAt:?Time.Time;
        timeStamp:Time.Time;
    };

    public type TreasuryActionRequest = {
        request:RequestDraft;
        title:Text;
        description:Text;
    };

    public type TreasuryAction = {
        id:Nat32;
        creator:Text;
        request:RequestDraft;
        title:Text;
        description:Text;
        yay:Nat;
        nay:Nat;
        executed:Bool;
        executedAt:?Time.Time;
        timeStamp:Time.Time;
    };

    public type Canister = {
        #dao;
        #treasury;
        #taxCollector;
        #swap;
    };

    public type Upgrade = {
        id:Nat32;
        creator:Text;
        wasm:Blob;
        args:Blob;
        canister:Canister;
        title:Text;
        description:Text;
        source:Text;
        hash:Text;
        yay:Nat;
        nay:Nat;
        executed:Bool;
        executedAt:?Time.Time;
        timeStamp:Time.Time;
    };

    public type UpgradeRequest = {
        wasm:Blob;
        args:Blob;
        canister:Canister;
        title:Text;
        description:Text;
        source:Text;
        hash:Text;
    };

    public type TreasuryRequest = {
        vote:Bool;
        title:Text;
        description:Text;
        treasuryRequestId:Nat32;
    };
    public type Treasury = {
        id:Nat32;
        treasuryRequestId:Nat32;
        creator:Text;
        vote:Bool;
        title:Text;
        description:Text;
        yay:Nat;
        nay:Nat;
        executed:Bool;
        executedAt:?Time.Time;
        timeStamp:Time.Time;
    };
}