//Nombre: Jesús Huerta Aguilar
//Matricula: 202041509

//Definir tamaño de la matriz
SubProceso tamMatriz(n Por Referencia)
	Escribir '-- DETECTOR DE MATRICES TRIANGULARES (nxn)-- '
	Escribir ''
	Escribir 'Ingresa el tamaño de la matriz (n > 1)'
	Repetir
		Leer n
		Si n <= 1 Entonces
			error
		FinSi
	Hasta Que n > 1
	Escribir ''
FinSubProceso

//Ingreso de valores
SubProceso ingresoMatriz(val Por Referencia,reclongval Por Referencia,gsup Por Referencia,tsup Por Referencia,ginf Por Referencia,tinf Por Referencia,n)
	Definir r,c,longval Como Entero
	Escribir '|||| Ingresar valores para la matriz A ||||'
	para r <- 1 hasta n Con Paso 1 Hacer
		Escribir '>> FILA ',r
		para c <- 1 hasta n con paso 1 hacer
			leer val[r,c]
			//longitud del valor
			longval <- Longitud(ConvertirATexto(val[r,c]))
			si longval > reclongval Entonces
				reclongval <- longval
			FinSi
			triangMatriz(gsup,tsup,ginf,tinf,val,r,c)
		FinPara
	FinPara
FinSubProceso

//determinar traiangularidad
SubProceso triangMatriz(gsup Por Referencia,tsup Por Referencia,ginf Por Referencia,tinf Por Referencia,val,r,c)
	si r > c Entonces
		gsup <- gsup + 1
		si val[r,c] = 0 Entonces
			tsup <- tsup + 1
		FinSi
	SiNo
		si r < c Entonces
			ginf <- ginf + 1
			si val[r,c] = 0 Entonces
				tinf <- tinf + 1
			FinSi
		FinSi
	FinSi
FinSubProceso

//Salida traiangularidad
SubProceso salidaMatriz(gsup,tsup,ginf,tinf)
	Escribir ' '
	Si tsup = gsup y tinf = ginf Entonces
		Escribir ' >> LA MATRIZ ES DIAGONAL <<'
	SiNo
		Si tsup = gsup Entonces
			Escribir ' >> LA MATRIZ ES TRIANGULAR SUPERIOR <<'
		SiNo
			Si tinf = ginf Entonces
				Escribir ' >> LA MATRIZ ES TRIANGULAR INFERIOR <<'
			SiNo
				Escribir ' >> LA MATRIZ NO ES TRIANGULAR <<'
			FinSi
		FinSi
	FinSi
FinSubProceso

//concatenación de espacios
SubProceso espacios(textvalf Por Referencia,reclongval,z,res,val,r,c)
	Definir e,switch Como Entero
	Definir iz,dr,x,textval Como Caracter
	iz <- ' '; dr <- ' '; x <- ' '
	switch <- 0
	para e <- 1 hasta reclongval+2 con paso 1 Hacer
		textval <- ConvertirATexto(val[r,c])
		textvalf <- Concatenar(Concatenar(iz,textval),dr)
		e <- Longitud(textvalf)
		si e < reclongval+2 y switch = 0 Entonces
			iz <- Concatenar(iz,x)
			switch <- 1
		SiNo
			dr <- Concatenar(x,dr)
			switch <- 0
		FinSi
	FinPara
FinSubProceso

//Imprimir matriz A
SubProceso crearMatriz(n,val)
	Definir r,c Como Entero
	para r <- 1 hasta n con paso 1 Hacer
		si r = redon(n/2) Entonces
			Escribir '    A = ' Sin Saltar
		SiNo
			Escribir '        ' Sin Saltar
		FinSi
		para c <- 1 hasta n con paso 1 Hacer
			espacios(textvalf,reclongval,z,res,val,r,c)
			Escribir '|',textvalf  Sin Saltar
		FinPara
		Escribir "|"
	FinPara
FinSubProceso

//Error
SubProceso error
	Escribir '[!] ERROR: VERIFICA TU INFORMACIÓN [!]'
	Escribir ''
FinSubProceso

//PROCESO PRINCIPAL
Proceso  mtrz_triang
	Definir n,reclongval,ginf,gsup,tinf,tsup Como Entero
	Definir val Como real
	Definir textvalf Como Caracter
	tamMatriz(n)
	Dimension val[n,n]
	ingresoMatriz(val,reclongval,gsup,tsup,ginf,tinf,n)
	salidaMatriz(gsup,tsup,ginf,tinf)
	crearMatriz(n,val)
FinProceso