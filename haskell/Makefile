# The compiler
HC=ghc
HCFLAGS=-O2 -threaded -Wall --make

# Documentation paths
DOC_URL=http://haskell.org/ghc/docs/latest/html/libraries
HDI_PATH=/usr/share/doc/ghc6-doc/libraries
HDI_HAXML_PATH=/usr/share/doc/haxml-doc
DOC_DIR=doc

# The documentation tool
HADDOCK=haddock
HADDOCK_FLAGS=-v -h -o $(DOC_DIR) --ignore-all-exports \
  -i $(DOC_URL)/base,$(HDI_PATH)/base/base.haddock \
  -i $(DOC_URL)/network,$(HDI_PATH)/network/network.haddock \
  -i $(DOC_URL)/stm,$(HDI_PATH)/stm/stm.haddock

# The output executable
OUTPUT=gaston

# Source files
SRC=gaston.hs \
    Logic.hs \
    Automaton.hs \
    GUIDisplay.hs


# Redundant files
REDUNDANT=*.hi *.o

# Files that should be removed
REMOVE=$(OUTPUT) \
       $(REDUNDANT)


#################################### RULES ####################################

all: $(OUTPUT) clean-redundant

$(OUTPUT): $(SRC)
	-$(HC) $(HCFLAGS) -o $@ $<

docs:
	$(HADDOCK) $(HADDOCK_FLAGS) $(SRC)

clean-redundant:
	rm -rf $(REDUNDANT)

clean:
	rm -rf $(REMOVE)
