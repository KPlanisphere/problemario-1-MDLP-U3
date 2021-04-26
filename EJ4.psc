//Nombre: Jesús Huerta Aguilar
//Matricula: 202041509

//Definir tamaño de las matrices
SubProceso tamMatriz(n Por Referencia)
	Escribir '-- DETECTOR DE MATRICES IDENTIDAD-- '
	Escribir 'NOTA: la matriz identidad es una matriz cuadrada (nxn)'
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
SubProceso ingresoMatriz(val Por Referencia,reclongval Por Referencia,diag Por Referencia,zero Por Referencia,n)
	Definir r,c,longval Como Entero
	reclongval <- 0
	Escribir '|||| Ingresar valores para la matriz ||||'
	para r <- 1 hasta n Con Paso 1 Hacer
		Escribir '>> FILA ',r
		para c <- 1 hasta n con paso 1 hacer
			leer val[r,c]
			//longitud del valor
			longval <- Longitud(ConvertirATexto(val[r,c]))
			si longval > reclongval Entonces
				reclongval <- longval
			FinSi
			identitad(diag,zero,val,r,c)
			
		FinPara
	FinPara
FinSubProceso

//Identificar Identidad
SubProceso identitad(diag Por Referencia,zero Por Referencia,val,r,c)
	si r = c Entonces
		Si val[r,c] = 1 Entonces
			diag <- diag + val[r,c]
		FinSi
	SiNo
		zero <- zero + val[r,c]
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

//Salida Identidad
SubProceso salida(diag,zero,n,val,reclongval)
	Definir i,r,c Como Entero
	Definir l,textvalf Como Caracter
	Escribir ''
	Si diag = n y zero = 0 Entonces
		Escribir ' >> LA MATRIZ ES IDENTIDAD <<'
		i = 1
	SiNo
		Escribir ' >> LA MATRIZ NO ES IDENTIDAD <<'
		i = 2
	FinSi
	Segun i
		1: l <- 'I'
		2: l <- 'A'
	FinSegun
	para r <- 1 hasta n con paso 1 Hacer
		si r = redon(n/2) Entonces
			Escribir '    ',l,' = ' Sin Saltar
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

//Proceso Principal
Proceso  identidad
	Definir n,reclongval Como Entero
	Definir diag,zero,val Como real
	tamMatriz(n)
	Dimension val[n,n]
	ingresoMatriz(val,reclongval,diag,zero,n)
	salida(diag,zero,n,val,reclongval)
FinProceso