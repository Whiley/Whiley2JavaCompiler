public export method test():
    {int f} x = {f: 0}
    int y = 0
    //
    x.f, y = 1,x.f
    //
    assert x == {f: 1}
    assert y == 0
    
