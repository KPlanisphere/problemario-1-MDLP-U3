//Nombre: Jesús Huerta Aguilar
//Matricula: 202041509

//Definir tamaño de las matrices
SubProceso tamMatriz(n Por Referencia,m Por Referencia)
	Escribir '-- SUMA DE DOS MATRICES RECTANGULARES DE REALES (nxm) --'
	Escribir 'Ingresar tamaño de las matrices (n,m > 1)'
	Repetir
		Escribir ''
		Escribir 'Cantidad de filas (n)'
		Leer n
		Escribir 'Cantidad de columnas (m)'
		Leer m
		Si n <= 1 o m <= 1 Entonces
			error
		FinSi
	Hasta Que n > 1 y m > 1
	Escribir ''
FinSubProceso

//Ingreso de valores para cada matriz
SubProceso registroMatriz(val Por Referencia,reclongval Por Referencia,n,m)
	Definir r,c,i,longval Como Entero
	Escribir '|||| Ingresar valores para la matriz A ||||'
	para i <- 1 hasta 2 con paso 1 Hacer
		Si i = 2 Entonces
			Escribir '|||| Ingresar valores para la matriz B ||||'
		FinSi
		para r <- 1 hasta n Con Paso 1 Hacer
			Escribir '>> FILA ',r
			para c <- 1 hasta m con paso 1 Hacer
				leer val[r,c,i]
				//longitud del valor
				longval <- Longitud(ConvertirATexto(val[r,c,i]))
				si longval > reclongval Entonces
					reclongval <- longval
				FinSi
			FinPara
		FinPara
		Escribir ''
	FinPara
FinSubProceso

//Creacion de matriz A y B
SubProceso crearMatriz(z Por Referencia,val,reclongval,n,m)
	Definir r,c,i Como Entero
	definir textvalf,l Como caracter
	Escribir '        //// RESULTADOS \\\\ '
	para i <- 1 hasta 2 con paso 1 Hacer
		Escribir ''
		Segun i
			1: 
				l <- 'A'
			2: 
				l <- 'B'
		FinSegun
		para r <- 1 hasta n con paso 1 Hacer
			si r = redon(n/2) Entonces
				Escribir '    ',l,' = ' Sin Saltar
			SiNo
				Escribir '        ' Sin Saltar
			FinSi
			para c <- 1 hasta m con paso 1 Hacer
				z <- 0
				espacios(textvalf,reclongval,z,res,val,r,c,i)
				Escribir '|',textvalf  Sin Saltar
			FinPara
			Escribir "|"
		FinPara
	FinPara
	Escribir '- - - - - - - - - - - - - - - - - - - - - - -  '
FinSubProceso

//concatenación de espacios
SubProceso espacios(textvalf Por Referencia,reclongval,z,res,val,r,c,i)
	Definir e,switch Como Entero
	Definir iz,dr,x Como Caracter
	iz <- ' '; dr <- ' '; x <- ' '
	switch <- 0
	para e <- 1 hasta reclongval+2 con paso 1 Hacer
		Si z = 0 Entonces
			textval <- ConvertirATexto(val[r,c,i])
		SiNo
			textval <- ConvertirATexto(res[r,c])
		FinSi
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

//Sumar matrices A + B
SubProceso sumarMatriz(res Por Referencia,reclongval Por Referencia,n,m,val)
	Definir r,c,switch,longval Como Entero
	para r <- 1 hasta n con paso 1 Hacer
		para c <- 1 hasta m con paso 1 Hacer
			res[r,c] <- val[r,c,1] + val[r,c,2]
			//longitud del valor
			longval <- Longitud(ConvertirATexto(res[r,c]))
			si longval > reclongval Entonces
				reclongval <- longval
			FinSi
		FinPara
	FinPara
FinSubProceso

//Crear matriz A+B
SubProceso crearMatrizAB(z Por Referencia,res,reclongval,n,m)
	definir r,c Como Entero
	Definir textvalf Como Caracter
	para r <- 1 hasta n Con Paso 1 Hacer
		si r = redon(n/2) Entonces
			Escribir 'A + B = ' Sin Saltar
		SiNo
			Escribir '        ' Sin Saltar
		FinSi
		para c <- 1 hasta m con paso 1 Hacer
			z <- 1
			espacios(textvalf,reclongval,z,res,val,r,c,i)
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
proceso suma_mtrcs_rectangulares
	Definir n,m,reclongval Como Entero
	Definir res,val Como Real
	Definir textval Como Caracter
	tamMatriz(n,m)
	Dimension val[n,m,2]; Dimension res[n,m]
	registroMatriz(val,reclongval,n,m)
	crearMatriz(z,val,reclongval,n,m)
	sumarMatriz(res,reclongval,n,m,val)
	crearMatrizAB(z,res,reclongval,n,m)
FinProceso