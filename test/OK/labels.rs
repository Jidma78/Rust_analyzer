fn f(x : i32) {
    let _x = 'a1 : loop { loop { if 2 < x { break 'a1 1;} else { break;}; }; };
    let _y : () = while x > 1 { if x > 56 { break; };  };
    let _z : i32 = 'label : { if x < 45 { break 'label 1;}; -2 };
    let _x = 'a : loop {
        while x != 0 { if x > 3 { break 'a 3; } else { break; };
        };
        break -2;
    };
}

fn main() {
    f(42);
}
