import Principal "mo:base/Principal";
import Nat64 "mo:base/Nat64";
import Constants "../Constants";

module {

    public type TxReceipt = {
        #Ok: Nat;
        #Err: {
            #InsufficientAllowance;
            #InsufficientBalance;
            #ErrorOperationStyle;
            #Unauthorized;
            #LedgerTrap;
            #ErrorTo;
            #Other: Text;
            #BlockUsed;
            #ActiveProposal;
            #AmountTooSmall;
        };
    };

    public func approve(spender:Principal, amount:Nat, canisterId:Text): async TxReceipt {
        let canister = actor(canisterId) : actor { 
            approve : shared (Principal, Nat) -> async TxReceipt;
        };
        await canister.approve(spender,amount);
    };

    public func allowance(owner:Principal, spender:Principal, canisterId:Text): async Nat {
        let canister = actor(canisterId) : actor { 
            allowance : shared query (Principal, Principal) -> async Nat;
        };
        await canister.allowance(owner, spender);
    };

    public func transfer(to:Principal, amount:Nat, canisterId:Text): async TxReceipt {
        let canister = actor(canisterId) : actor { 
            transfer: (Principal, Nat)  -> async TxReceipt;
        };
        await canister.transfer(to, amount);
    };

    public func transferFrom(from:Principal, to:Principal, amount:Nat, canisterId:Text): async TxReceipt {
        let canister = actor(canisterId) : actor { 
            transferFrom : shared (Principal, Principal, Nat) -> async TxReceipt;
        };
        await canister.transferFrom(from, to, amount);
    };

}