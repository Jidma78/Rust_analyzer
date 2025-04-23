fn f(x : i32) {
    let _z : i32 = 'label : { if x < 45 { break 'label 1;}; -2.0 };
}

fn main() {
    f(42);
}
