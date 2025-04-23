fn f () {
    fn f(x : i32) {
        let _ = x;
        f()
    }
}

fn main() {
    f();
}
