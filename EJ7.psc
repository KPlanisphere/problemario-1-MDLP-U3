//Nombre: Jesús Huerta Aguilar
//Matricula: 202041509

//Definir tamaño de la matriz
SubProceso tamMatriz(n Por Referencia,m Por Referencia)
	Escribir '-- SUMA POR COLUMNAS DE MATRICES nxm --'
	Escribir 'Ingresar tamaño de las matrices (n,m > 1)'
	Repetir
		//tamaño de la matriz
		Escribir ''
		Escribir 'Cantidad de filas (n)'
		Leer n
		Escribir 'Cantidad de columnas (m)'
		Leer m
		Si n <= 1 o m <= 1 Entonces
			error
		FinSi
	Hasta Que n > 1 y m > 1
FinSubProceso

//Ingreso de valores
SubProceso ingresoMatriz(val Por Referencia,reclongval Por Referencia,n,m)
	Definir r,c,longval Como Entero
	Escribir '|||| Ingresar valores para la matriz A ||||'
	para r <- 1 hasta n Con Paso 1 Hacer
		Escribir '>> FILA ',r
		para c <- 1 hasta m con paso 1 hacer
			leer val[r,c]
			//longitud del valor
			longval <- Longitud(ConvertirATexto(val[r,c]))
			si longval > reclongval Entonces
				reclongval <- longval
			FinSi
		FinPara
	FinPara
FinSubProceso

//concatenación de espacios
SubProceso espacios(textvalf Por Referencia,reclongval,res,val,r,c)
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
SubProceso crearMatriz(val,reclongval,n,m)
	Definir r,c Como Entero
	Definir textvalf Como Caracter
	Escribir ''
	para r <- 1 hasta n con paso 1 Hacer
		si r = redon(n/2) Entonces
			Escribir '    A = ' Sin Saltar
		SiNo
			Escribir '        ' Sin Saltar
		FinSi
		para c <- 1 hasta m con paso 1 Hacer
			espacios(textvalf,reclongval,res,val,r,c)
			Escribir '|',textvalf  Sin Saltar
		FinPara
		Escribir "|"
	FinPara
FinSubProceso

//suma de columnas
SubProceso columnaMatriz(scol Por Referencia,val,n,m)
	Definir r,c Como Entero
	Escribir ''
	Escribir '>> SUMA POR COLUMNAS <<'
	para c <- 1 hasta m con paso 1 Hacer
		para r <- 1 hasta n con paso 1 Hacer
			scol[c] <- scol[c] + val[r,c]
		FinPara
		Escribir 'Columna ',c,': ',scol[c]
	FinPara
FinSubProceso

//suma total de columnas
SubProceso totalMatriz(scol,n,m)
	Definir r,c Como Entero
	Definir sfinal Como Real
	Escribir ''
	Escribir '>> SUMA TOTAL DE COLUMNAS <<'
	para c <- 1 hasta m con paso 1 Hacer
		sfinal <- sfinal + scol[c]
	FinPara
	Escribir 'Suma total: ',sfinal
FinSubProceso

//Error
SubProceso error
	Escribir '[!] ERROR: VERIFICA TU INFORMACIÓN [!]'
	Escribir ''
FinSubProceso

//PROCESO PRINCIPAL
Proceso  suma_columna
	Definir n,m,reclongval Como Entero
	Definir val,scol Como real
	tamMatriz(n,m)
	Dimension val[n,m]
	Dimension scol[m]
	ingresoMatriz(val,reclongval,n,m)
	crearMatriz(val,reclongval,n,m)
	columnaMatriz(scol,val,n,m)
	totalMatriz(scol,n,m)
FinProceso