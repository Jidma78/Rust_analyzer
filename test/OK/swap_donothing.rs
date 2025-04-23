fn swap(mut x : i32, mut y : i32) {
    let t = x;
    x = y;
    y = t;
}

fn main() {
    let mut x = 1;
    let mut y = 2;
    swap(x, y); // OK, however, does nothing
}
