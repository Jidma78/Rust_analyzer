fn even(x : i32) -> bool {
    x == 0 || odd(x - 1)
}

fn odd(x : i32) -> bool {
    x != 0 && even(x - 1)
}

fn main() {
    even(42);
}
    
