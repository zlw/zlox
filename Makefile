TEST_FILES=`find test -name "*.lox" \
	| grep -v test/benchmark \
	| grep -v test/scanning \
	| grep -v test/expressions \
	`
default: repl

clean:
	rm -rf build/*
	rm -rf zig-out/*
	rm -rf zig-cache
	rm -rf src/zig-cache

build:
	zig build

release:
	zig build -Doptimize=ReleaseFast

unit:
	zig test src/main.zig

system: build
	zig run util/test.zig -- zig-out/bin/zlox $(TEST_FILES)

repl: build
	./zig-out/bin/zlox
