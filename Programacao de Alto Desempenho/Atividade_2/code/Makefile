CXX = g++
CXXFLAGS = -O3 -march=native -std=c++17 -funroll-loops
LDFLAGS = -lblas -lopenblas
SRCDIR = src
BINDIR = bin
SOURCES = $(SRCDIR)/main.cpp $(SRCDIR)/matmul_orders.cpp $(SRCDIR)/matmul_blocking.cpp $(SRCDIR)/strassen.cpp $(SRCDIR)/blas_wrapper.cpp
OBJECTS = $(SOURCES:.cpp=.o)

all: $(BINDIR)/matmul_exec

$(BINDIR)/matmul_exec: $(SOURCES)
	mkdir -p $(BINDIR)
	$(CXX) $(CXXFLAGS) -I$(SRCDIR) $(SOURCES) -o $@ $(LDFLAGS)

clean:
	rm -f $(BINDIR)/matmul_exec *.o src/*.o

.PHONY: all clean
