.PHONY: all stat-loc clean

V       ?= @
E_ENCODE = $(shell echo $(1) | sed -e 's!_!_1!g' -e 's!/!_2!g')
E_DECODE = $(shell echo $(1) | sed -e 's!_2!/!g' -e 's!_1!_!g')
T_BASE   = target

T_CC_FLAGS_OPT ?= -O0 -g 
T_CC_FLAGS     ?= ${T_CC_FLAGS_OPT} -Wall -Iruntime

RT_SRCFILES:= $(shell find runtime '(' '!' -regex '\./_.*' ')' -and '(' -iname "*.c" ')' | sed -e 's!\./!!g')
RT_SRCFILES:= $(filter-out simple_interpreter.c, ${RT_SRCFILES})
RT_OBJFILES:= $(addprefix ${T_BASE}/,$(addsuffix .o,$(foreach FILE,${RT_SRCFILES},$(call E_ENCODE,${FILE}))))
RT_DEPFILES:= $(RT_OBJFILES:.o=.d)

all: ${T_BASE}/rt.a

-include ${RT_DEPFILES}

${T_BASE}/%.d:
	${V}${CC} $(call E_DECODE,$*) -o $@ -MM $(T_CC_FLAGS) -MT $(@:.d=.o)

${T_BASE}/%.o: ${T_BASE}/%.d
	@echo CC $(call E_DECODE,$*)
	${V}${CC} $(call E_DECODE,$*) -o $@ ${T_CC_FLAGS} -c

${T_BASE}/rt.a: ${RT_OBJFILES}
	@echo AR $@
	${V}ar r $@ $^

stat-loc:
	${V}wc ${RT_SRCFILES} -l

clean:
	-${V}rm -f ${T_BASE}/*
