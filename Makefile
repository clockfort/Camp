all: camp-bin

camp-bin: camp-core camp-network camp-repository camp-bin/camp.cabal
	cd camp-bin;\
	runhaskell Setup configure;\
	runhaskell Setup build;\
	runhaskell Setup install

camp-core: camp-core/camp-core.cabal
	cd camp-core;\
	runhaskell Setup configure;\
	runhaskell Setup build;\
	runhaskell Setup install

camp-fragment: camp-repository camp-core camp-fragment/camp-fragment.cabal
	cd camp-fragment;\
	runhaskell Setup configure;\
	runhaskell Setup build;\
	runhaskell Setup install

camp-repository: camp-network camp-core camp-repository/camp-repository.cabal
	cd camp-repository;\
	runhaskell Setup configure;\
	runhaskell Setup build;\
	runhaskell Setup install

camp-network: camp-core camp-network/camp-network.cabal
	cd camp-network;\
	runhaskell Setup configure;\
	runhaskell Setup build;\
	runhaskell Setup install
