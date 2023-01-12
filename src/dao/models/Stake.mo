import Time "mo:base/Time";

module {
    public type Stake = {
        amount:Nat;
        timeStamp:?Time.Time;
    }
}