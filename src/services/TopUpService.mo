import Principal "mo:base/Principal";
import Utils "../helpers/Utils";
import Nat64 "mo:base/Nat64";
import Constants "../Constants";
import Result "mo:base/Result";
import Request "../dao/models/Request";

module {

    public func topUp(): async () {
        await canister.topUp();
    };

    private let canister = actor(Constants.topUpCanister) : actor { 
        topUp : shared () -> async ();
    };
    
}