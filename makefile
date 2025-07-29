all:
	ghc --make Main.hs
	rm -rf *.hi *.o
clean:
	rm -rf *.hi *.o *.csv resultados/*.txt Main
test: all
	./Main < testes/1k1.txt
	./Main < testes/1k2.txt
	./Main < testes/1k3.txt
	./Main < testes/1k4.txt
	./Main < testes/1k8.txt
	./Main < testes/2k2.txt
	./Main < testes/2k3.txt
	./Main < testes/2k4.txt
	./Main < testes/2k5.txt
	./Main < testes/3k2.txt
	./Main < testes/3k3.txt
	./Main < testes/3k5.txt
diff: test
	python3 correcao.py resultados/base1k1.txt Validacao/resultados/base1k1.txt
	python3 correcao.py resultados/base1k2.txt Validacao/resultados/base1k2.txt
	python3 correcao.py resultados/base1k3.txt Validacao/resultados/base1k3.txt
	python3 correcao.py resultados/base1k4.txt Validacao/resultados/base1k4.txt
	python3 correcao.py resultados/base1k8.txt Validacao/resultados/base1k8.txt
	python3 correcao.py resultados/base2k2.txt Validacao/resultados/base2k2.txt
	python3 correcao.py resultados/base2k3.txt Validacao/resultados/base2k3.txt
	python3 correcao.py resultados/base2k4.txt Validacao/resultados/base2k4.txt
	python3 correcao.py resultados/base2k5.txt Validacao/resultados/base2k5.txt
	python3 correcao.py resultados/base3k2.txt Validacao/resultados/base3k2.txt
	python3 correcao.py resultados/base3k3.txt Validacao/resultados/base3k3.txt
	python3 correcao.py resultados/base3k5.txt Validacao/resultados/base3k5.txt