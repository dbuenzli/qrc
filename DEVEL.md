# Self test vectors

If you are making changes to the library that must preserve QR matrix
outputs, make sure the following test passes:

    git checkout -b my-changes
    topkg build
    topkg run self_test -- --gen > test/self_vecs.ml
    # ... Make your changes 
    topkg build 
    topkg test self_test 


