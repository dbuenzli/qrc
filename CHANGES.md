
- Require OCaml 4.14.0.

- `qrtrip` tool. Fix `stdin` reading bug, newlines were being dropped
  and arbitrary binary input was impossible (#5).

- `qrtrip` tool. Incompatible change to fix the footgun UI. Before,
  the optional positional argument was the message to QR encode, but
  it feels natural to specify a file to QR encode. In which case you
  would silently QR encode a file path… The optional argument is now a
  file to read by default. Use option `-m/--message` to treat the
  argument as the message and make the tool operate as it used to.
    
- `Qrc.Matrix.to_svg`, change matrix rendering strategy. The result doesn't
  use xlink (no risk of identifier clashes when embedding), renders faster in
  browsers and is more compact (#1). Thanks to Alain Frisch for the patch.

v0.1.0 2020-10-22 Zagreb
------------------------

First release.
