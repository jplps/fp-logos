CHEZ = chez
LIBDIRS = .

TESTS = $(wildcard tests/*-test.ss)

.PHONY: test run clean

test:
	@for f in $(TESTS); do \
		echo "--- $$f ---"; \
		$(CHEZ) --libdirs $(LIBDIRS) --script $$f; \
	done

run:
	$(CHEZ) --libdirs $(LIBDIRS) src/core.ss

clean:
	find . -name '*.so' -delete
	find . -name '*.wpo' -delete
