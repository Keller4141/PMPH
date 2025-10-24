NVCC     ?= nvcc
ARCH     ?= -arch=sm_80
# CUB_DIR  ?= ./cub-1.8.0      # <-- fjern/kommentér

# INC_FLAGS := -I$(CUB_DIR)    # <-- fjern/kommentér
INC_FLAGS :=                   # <-- tomt
NVCCFLAGS := $(ARCH) -O3 --std=c++14

BIN := test-rank-search-k
SRC := segmented_kselect_cub.cu
HDR := utils.cuh

$(BIN): $(SRC) $(HDR)
	$(NVCC) $(NVCCFLAGS) $(INC_FLAGS) -o $@ $(SRC)

run: $(BIN)
	@echo "==> Running default N,M series"
	@./$(BIN) 6250000    1000
	@./$(BIN) 12500000   1000
	@./$(BIN) 25000000   1000
	@./$(BIN) 50000000   1000
	@./$(BIN) 100000000  1000
	@./$(BIN) 200000000  1000
	@./$(BIN) 400000000  1000

# Kør med vilkårlige størrelser: make run-NM N=<total> M=<segments>
run-NM: $(BIN)
	@if [ -z "$(N)" ] || [ -z "$(M)" ]; then \
	  echo "Usage: make run-NM N=<total> M=<segments>"; exit 1; \
	fi
	./$(BIN) $(N) $(M)

clean:
	rm -f $(BIN)