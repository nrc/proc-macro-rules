# proc-macro-rules

Emulate macro-rules pattern matching in proc macros.

```rust


rules!(tokens => {
    ($finish:ident ($($found:ident)*) # [ $($inner:tt)* ] $($rest:tt)*) => {

    }
    _ => foo(),
})

```

ident.parse
while
    ident.parse
tok.parse
tree.parse
    while
        tt.parse
while
    tt.parse


var
repeat
opt
token
tree
