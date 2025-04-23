fn f() {
    fn g() {
        fn f() {
        }

        f()
    }

    f()
}

fn main() {
    f()
}
