type ur4nat is int
type tur4nat is int

type wur4nat is ur4nat | tur4nat

function f(wur4nat x) -> int:
    return x

public export method test() :
    assume f((tur4nat) 1) == 1
