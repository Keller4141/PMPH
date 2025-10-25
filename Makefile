NVCC     ?= nvcc
ARCH     ?= -arch=sm_80
# CUB_DIR  ?= ./cub-1.8.0

# INC_FLAGS := -I$(CUB_DIR)
INC_FLAGS := 
NVCCFLAGS := $(ARCH) -O3 --std=c++14

BIN := test-rank-search-k
SRC := segmented_kselect_cub.cu
HDR := utils.cuh

$(BIN): $(SRC) $(HDR)
	$(NVCC) $(NVCCFLAGS) $(INC_FLAGS) -o $@ $(SRC)

run: $(BIN)
	@echo "==> Running reduced N,M series"
	@./$(BIN) 5000000    1000
	@./$(BIN) 10000000   1000
	@./$(BIN) 20000000   1000
	@./$(BIN) 40000000   1000
	@./$(BIN) 80000000   1000
	@./$(BIN) 160000000  1000
	@./$(BIN) 320000000  1000

run-NM: $(BIN)
	@if [ -z "$(N)" ] || [ -z "$(M)" ]; then \
	  echo "Usage: make run-NM N=<total> M=<segments>"; exit 1; \
	fi
	./$(BIN) $(N) $(M)

clean:
	rm -f $(BIN)