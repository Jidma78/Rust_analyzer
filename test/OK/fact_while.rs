fn fact(n : i32) -> i32 {
    let mut i = 2;
    let mut f = 1;
    while i <= n {
        f = f * i;
        i = i + 1;
    };
    f
}

fn main() {
    let _ = fact(10);
}
