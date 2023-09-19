all: build run

build:
	cargo build

run:
	./target/debug/rust-token-editor

test:
	cargo test

testv:
	cargo test -- --nocapture
