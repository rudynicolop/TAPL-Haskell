# Build and Run

To run an STLC program, do:
```bash
cabal new-run stlc -- tests/simple.stlc
```

## Common Errors
If you are get confusing error messages, it may be because you forgetting to add types to `Left` and `Right` like:
```
Left t1 + t2 e
Right t1 + t2 e
```
