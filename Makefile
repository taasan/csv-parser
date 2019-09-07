build bench clean test:
	stack $@

all: test bench

setup: .git/hooks/pre-commit

HOOK_DIR = .git/hooks

HOOKS = $(HOOK_DIR)/pre-commit $(HOOK_DIR)/pre-push

$(HOOKS) : git-hooks/common.sh
	ln -s ../../$< $@

install-hooks: $(HOOKS)

pre-commit: test bench

pre-push: test bench

format:
	find . -path ./.stack-work -prune -o -name \*.hs \
		-exec stack exec -- hindent --sort-imports {} \; \
		-exec stack exec -- stylish-haskell -i {} \;

.PHONY: all \
	build \
	clean \
	test \
	bench \
	setup \
	pre-\
	commit \
	pre-\
	push \
	install-\
	hooks \
	format
