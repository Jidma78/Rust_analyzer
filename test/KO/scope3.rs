fn f (mut x : i32) {
    fn g() { x = 12 ; }
    g();
}

fn main() {
    let mut x = 1;
    f(x);
}
