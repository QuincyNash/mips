CC = gcc
CFLAGS = -g -Wall -Wextra -Werror -MMD -MP
# -fsanitize=address -fsanitize=undefined
BUILD_DIR = build

# Source files
SRCS = cpu.c assembler.c tables.c helper.c array.c opcodes.c
OBJS = $(patsubst %.c,$(BUILD_DIR)/%.o,$(SRCS))
DEPS = $(OBJS:.o=.d)

# Executable targets in build directory
EXEC_COMPILE = $(BUILD_DIR)/compile
EXEC_LOAD    = $(BUILD_DIR)/load

all: $(EXEC_COMPILE) $(EXEC_LOAD)

# Build executables from object files
$(EXEC_COMPILE): $(OBJS) $(BUILD_DIR)/compile.o
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(BUILD_DIR)/compile.o

$(EXEC_LOAD): $(OBJS) $(BUILD_DIR)/load.o
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(BUILD_DIR)/load.o

# Pattern rule for all .o files
$(BUILD_DIR)/%.o: %.c | build_dir
	$(CC) $(CFLAGS) -c $< -o $@ -MF $(BUILD_DIR)/$*.d

# Include auto-generated dependencies
-include $(DEPS)

build: $(BUILD_DIR)/compile $(BUILD_DIR)/load
	@echo "Build complete."

# Convenience targets
compile: $(EXEC_COMPILE)
	$<

load: $(EXEC_LOAD)
	$<

leaks_compile: $(EXEC_COMPILE)
	leaks --atExit -- $<

leaks_load: $(EXEC_LOAD)
	leaks --atExit -- $<

clean:
	rm -rf $(BUILD_DIR)

.PHONY: build_dir
build_dir:
	mkdir -p $(BUILD_DIR)