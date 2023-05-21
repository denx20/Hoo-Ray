Contains testing files for UDP multicast from [this Gist](https://gist.github.com/hostilefork/f7cae3dc33e7416f2dd25a402857b6c6).

Run with

```bash
# Terminal window 1
make
./sender 239.255.255.250 1900

# Terminal window 2
./listener 239.255.255.250 1900
```

Expected behavior: Lines of "Hello World" printed on the terminal of `listener`.
