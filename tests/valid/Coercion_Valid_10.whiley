type pos is (int x) where x >= 0
type neg is (int x) where x < 0

function f(pos x) -> (pos[]|neg[] r):
    return [x]

function g(neg x) -> (pos[]|neg[] r):
    return [x]

public export method test():
    //
    assert f(1) == [1]
    //
    assert g(-1) == [-1]    

