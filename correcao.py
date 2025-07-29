import csv
import argparse

def ler_grupos(nome_arquivo: str) -> list[set[int]]:
    grupos = []
    with open(nome_arquivo, 'r') as f:
        leitor = csv.reader(f)
        for linha in leitor:
            grupo = set(int(x.strip()) for x in linha if x.strip())
            if grupo:
                grupos.append(grupo)
    return grupos

def comparar_grupos(arq_saida: str, arq_referencia: str) -> bool:
    grupos_saida = ler_grupos(arq_saida)
    grupos_ref = ler_grupos(arq_referencia)

    conjunto_saida = set(frozenset(g) for g in grupos_saida)
    conjunto_ref = set(frozenset(g) for g in grupos_ref)

    if conjunto_saida == conjunto_ref:
        print("Os grupos estão corretos!")
        return True
    else:
        print("Os grupos NÃO estão corretos.")
        faltando = conjunto_ref - conjunto_saida
        extras = conjunto_saida - conjunto_ref
        if faltando:
            print("\nGrupos esperados mas ausentes:")
            for g in faltando:
                print(", ".join(map(str, sorted(g))))
        if extras:
            print("\nGrupos presentes mas não esperados:")
            for g in extras:
                print(", ".join(map(str, sorted(g))))
        return False

def main():
    parser = argparse.ArgumentParser(description="Comparar agrupamentos de pontos.")
    parser.add_argument("saida", help="Arquivo de saída gerado pelo programa")
    parser.add_argument("gabarito", help="Arquivo de referência (gabarito)")
    args = parser.parse_args()

    comparar_grupos(args.saida, args.gabarito)

if __name__ == "__main__":
    main()