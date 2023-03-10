import Principal "mo:base/Principal";
import Utils "../helpers/Utils";
import Nat64 "mo:base/Nat64";
import Constants "../Constants";
import Result "mo:base/Result";
import Request "../dao/models/Request";

module {

    private type ErrorMessage = { #message : Text;};
    private type RequestDraft = Request.RequestDraft;

    public func approveRequest(id : Nat32, canisterId:Text): async Result.Result<(), ErrorMessage> {
        let canister = actor(canisterId) : actor { 
            approveRequest : shared (Nat32) -> async Result.Result<(), ErrorMessage>;
        };
        await canister.approveRequest(id);
    };

    public func createRequest(proposalId:Nat32,request : RequestDraft, canisterId:Text): async Nat32 {
        let canister = actor(canisterId) : actor { 
            createRequest : shared (Nat32, RequestDraft) -> async Nat32;
        };
        await canister.createRequest(proposalId,request);
    };
}