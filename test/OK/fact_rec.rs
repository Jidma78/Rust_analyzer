fn fact(n : i32) -> i32 {
    let r = if n == 0 { 1 }
    else { n * fact(n - 1) };
    r
}

fn main() {
    let _ = fact(10);
}
