import Time "mo:base/Time";

module {
    public type Vote = {
        proposalId:Nat32;
        yay:Bool;
        member:Text;
        power:Float;
        timeStamp:Time.Time;
    }
}