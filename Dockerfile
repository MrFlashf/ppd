FROM haskell:8

RUN mkdir /ppd
WORKDIR /ppd
RUN ghci cw1_krzysztof_jozefowicz.hs
