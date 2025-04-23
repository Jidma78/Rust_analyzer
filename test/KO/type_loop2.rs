fn f(x : i32) {
    let _z : i32 = 'label : { if x + 7 < x * x { break 'label 1;} else
                            { break 'label 2.0; }; 2 };
}

fn main() {
    f(42);
}
