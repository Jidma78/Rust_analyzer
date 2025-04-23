fn fact(n : i32) -> i32 {
    let mut i = 2;
    let mut f = 1;
    let r = loop {
        if i > n { break f };
        f = f * i;
        i = i + 1;
    };
    r
}

fn main() {
    let _ = fact(10);
}
