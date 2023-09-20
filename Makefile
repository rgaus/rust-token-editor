all: build run

build:
	cargo build

run:
	./target/debug/rust-token-editor

test:
	time cargo test

testv:
	time cargo test -- --nocapture
