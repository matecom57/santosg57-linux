import subprocess

def Mani(dd=''):
  resultado = subprocess.run(["man", dd], capture_output=True, text=True)

  # Mostrar la salida estándar (stdout)
  filout = open(dd+'.txt', 'w')
  filout.write(resultado.stdout)
  filout.close()


file = 'aaa.txt'

fil = open(file, 'r')

datos = fil.read()

datos = datos.replace('\n', '')
datos = datos.split(' ')
datos = list(set(datos))

for ss in datos:
  if len(ss) > 0:
    print(ss)
    Mani(ss)



