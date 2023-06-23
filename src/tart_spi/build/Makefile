COUNTER_BITS	?= 12

.PHONY:	out run
run:	out
	@COUNTER_BITS=$(COUNTER_BITS) time ./tart.out

out:
	@mkdir -p vcd
