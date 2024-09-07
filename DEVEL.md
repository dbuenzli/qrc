# Tests

    b0 test


If you are making changes to the library that must preserve QR matrix
outputs, make sure the following test passes:

    git checkout -b my-changes
    b0 -- test_vecs --gen 
    # … make changes …
    b0 test


