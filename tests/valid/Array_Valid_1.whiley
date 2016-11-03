type in_arr is ((int|null)[] n)
type ib_arr is ((int|bool)[] n)
type arr is in_arr | ib_arr

function read(arr x, int i) -> (int|null|bool r)
requires i >= 0 && i < |x|:
    //
    return x[i]

function write(arr x, int i, int n) -> (arr r)
requires i >= 0 && i < |x|:
    //
    x[i] = n
    //
    return x

public export method test():
    //
    arr a = [1,null,3]    
    assume read(a,0) == 1
    assume read(a,1) == null
    //
    assume write(a,1,2) == [1,2,3]
