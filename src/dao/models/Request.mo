import Time "mo:base/Time";
import TrieMap "mo:base/TrieMap";

module {
    
    public type Request = {
        #swapFor:Transfer;
        #withdrawLiquidity:WithdrawLiquidity;
        #addLiquidity:Transfer;
        #transfer:Transfer;
        #addMember:Member;
        #removeMember:Member;
        #threshold:Threshold;
    };

    public type RequestResponse = {
        #swapFor:TransferResponse;
        #withdrawLiquidity:WithdrawLiquidityResponse;
        #addLiquidity:TransferResponse;
        #transfer:TransferResponse;
        #addMember:MemberResponse;
        #removeMember:MemberResponse;
        #threshold:ThresholdResponse;
    };

    public type WithdrawLiquidity = {
        id:Nat32;
        proposalId:Nat32;
        amount:Nat;
        approvals:TrieMap.TrieMap<Text, Nat>;
        executed:Bool;
        createdAt:Time.Time;
        executedAt:?Time.Time;
        description:Text;
        error:?Text;
    };

    public type WithdrawLiquidityResponse = {
        id:Nat32;
        proposalId:Nat32;
        amount:Nat;
        executed:Bool;
        createdAt:Time.Time;
        executedAt:?Time.Time;
        description:Text;
        error:?Text;
    };

    public type Transfer = {
        id:Nat32;
        proposalId:Nat32;
        token:Token;
        amount:Nat;
        recipient:Text;
        approvals:TrieMap.TrieMap<Text, Nat>;
        executed:Bool;
        createdAt:Time.Time;
        executedAt:?Time.Time;
        description:Text;
        error:?Text;
    };

    public type TransferResponse = {
        id:Nat32;
        proposalId:Nat32;
        token:Token;
        amount:Nat;
        recipient:Text;
        executed:Bool;
        createdAt:Time.Time;
        executedAt:?Time.Time;
        description:Text;
        error:?Text;
    };

    public type Member = {
        id:Nat32;
        proposalId:Nat32;
        principal:Text;
        power:Nat;
        description:Text;
        approvals:TrieMap.TrieMap<Text, Nat>;
        executed:Bool;
        createdAt:Time.Time;
        executedAt:?Time.Time;
        error:?Text;
    };

    public type MemberResponse = {
        id:Nat32;
        proposalId:Nat32;
        principal:Text;
        power:Nat;
        description:Text;
        executed:Bool;
        createdAt:Time.Time;
        executedAt:?Time.Time;
        error:?Text;
    };

    public type Threshold = {
        id:Nat32;
        proposalId:Nat32;
        power:Nat;
        description:Text;
        approvals:TrieMap.TrieMap<Text, Nat>;
        executed:Bool;
        createdAt:Time.Time;
        executedAt:?Time.Time;
        error:?Text;
    };

    public type ThresholdResponse = {
        id:Nat32;
        proposalId:Nat32;
        power:Nat;
        description:Text;
        executed:Bool;
        createdAt:Time.Time;
        executedAt:?Time.Time;
        error:?Text;
    };

    public type RequestDraft = {
        #swapFor:TransferDraft;
        #withdrawLiquidity:WithdrawLiquidityDraft;
        #addLiquidity:TransferDraft;
        #transfer:TransferDraft;
        #addMember:MemberDraft;
        #removeMember:MemberDraft;
        #threshold:ThresholdDraft;
    };

    public type WithdrawLiquidityDraft = {
        amount:Nat;
        recipient:Text;
        description:Text;
    };


    public type TransferDraft = {
        token:Token;
        amount:Nat;
        recipient:Text;
        description:Text;
    };
    public type MemberDraft = {
        principal:Text;
        power:Nat;
        description:Text;
    };
    public type ThresholdDraft = {
        power:Nat;
        description:Text;
    };

    public type Token = {
        #token;
        #icp;
    };
}