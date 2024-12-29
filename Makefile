default: repl

clean:
	rm -rf build/*
	rm -rf zig-out/*
	rm -rf zig-cache
	rm -rf .zig-cache
	rm -rf src/zig-cache

build:
	zig build

release:
	zig build -Doptimize=ReleaseFast

cross_release:
	zig build -Doptimize=ReleaseFast -Dtarget="x86_64-$(target)" && cp ./zig-out/bin/zlox ./zig-out/bin/zlox-x86_64-$(target)

unit:
	zig test src/main.zig

system: release
	zig run util/test.zig -- zig-out/bin/zlox `find $(path) -name "*.lox"`

repl: build
	./zig-out/bin/zlox

run: build
	./zig-out/bin/zlox $(path)

bench: release
	./zig-out/bin/zlox $(path)
	../craftinginterpreters/clox $(path)
