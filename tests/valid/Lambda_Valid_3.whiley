type t_Reader is method(int) -> byte[]
type InputStream is { t_Reader read }
type BufferState is &{byte[] bytes, int pos}

// Define the 8bit ASCII character
public type char is (int x) where 0 <= x && x <= 255

// Define ASCII String
type string is char[]

// Convert an ASCII character into a byte.
public function toByte(char v) -> byte:
    //
    byte mask = 0b00000001
    byte r = 0b0
    int i = 0
    while i < 8:
        if (v % 2) == 1:
            r = r | mask
        v = v / 2
        mask = mask << 1
        i = i + 1
    return r

// Convert an ASCII string into a list of bytes
public function toBytes(string s) -> byte[]:
    byte[] r = [0b0; |s|]
    int i = 0
    while i < |s| where i >= 0:
        r[i] = toByte(s[i])
        i = i + 1
    return r

// Compute minimum of two integers
function min(int a, int b) -> int:
    if a <= b:
        return a
    else:
        return b

// Read specified number of bytes from buffer
method read(BufferState state, int amount) -> byte[]
requires amount >= 0:
    //
    byte[] r = [0b0; amount]
    int i = 0
    //
    while i < amount && state->pos < |state->bytes|:
        r[i] = state->bytes[state->pos]
        state->pos = state->pos + 1
        i = i + 1
    //
    return r

// Construct buffer from list of bytes
public method BufferInputStream(byte[] buffer) -> InputStream:
    BufferState _this = new {bytes: buffer, pos: 0}
    return {read: &(int x -> read(_this, x))}

public export method test() :
    InputStream bis = BufferInputStream(toBytes("hello"))
    byte[] bytes = bis.read(7)
    assume bytes == [0b01101000, 0b01100101, 0b01101100, 0b01101100, 0b01101111, 0b0, 0b0]
    //
    bis = BufferInputStream(toBytes("cruel world"))
    bytes = bis.read(7)    
    assume bytes == [0b01100011, 0b01110010, 0b01110101, 0b01100101, 0b01101100, 0b00100000, 0b01110111]
