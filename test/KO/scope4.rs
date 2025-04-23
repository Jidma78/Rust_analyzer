fn f () {
    let mut x = 1;
    fn g() { x = 12 ; }
    g();
}

fn main() {
    f();
}
