C   Utec Tulancingo
C   Matematicas para ingenieria II
C   RESUELVE UN P.V.I. CON UNA ECUACION DIFERENCIAL ORDINARIA DE
C   PRIMER ORDEN POR EL METODO DE Euler Modificado

C   DESCRIPCION DE VARIABLES :

C   X     : VALOR INICIAL DE LA VARIABLE INDEPENDIENTE
C   XN    : VALOR FINAL DE LA VARIABLE DEPENDIENTE
C   H     : PASO DE INTEGRACION
C   Y     : VALOR DE LA VARIABLE INDEPENDIENTE EN X
C   NPRINT: CONTROL DE IMPRESION
C           (SE IMPRIMEN X y Y CADA NPRINT ITERACIONES)

      Program EULER_1
      Call Lee_Datos(X,Xn,H,Y,Nprint)
      Call Escribe_Datos(X,XN,H,Y,Nprint)
      Call EULER(X,Y,Xn,H,Nprint)
      End


C    SE DEFINE LA FUNCION :

      Function F(X,Y)
            F = ((X**2) + (Y/X))/(-(LOG(X)) - (2*Y))
      Return
      End

C   Lectura de los datos

C      Subroutine Lee_Datos(X,XN,N,Y,Nprint)
      Subroutine Lee_Datos(X,XN,H,Y,Nprint)
C   Las siguientes instrucciones permiten leer datos desde el teclado

      Write(*,*)'   Valor inicial de la variable independiente X0 '
      Read(*,*)X
      Write(*,*)'   Valor final de la variable independiente   XN '
      Read(*,*)Xn
      Write(*,*)'   Valor inicial de la variable dependiente   Y0 '
      Read(*,*)Y
      Write(*,*)'   Paso de integracion        H  '
      Read(*,*)H
      Write(*,*)'   Control de impresi�n NPRINT                   '
      Read(*,*)Nprint


      
      End

C   Escritura de valores iniciales

      Subroutine Escribe_Datos(X,XN,H,Y,Nprint)

      Write(*,*)'   CONDICION INICIAL :     Y(',X,')= ',Y
      Write(*,*)'   PASO DE INTEGRACION H         = ',H
      Write(*,*)'   VALOR FINAL DE X              = ',XN
      Write(*,*)'   SE ESCRIBE CADA ',NPRINT,' ITERACIONES'
      Write(*,*)'         X             Y     '
      Write(*,*)'    __________      __________'
      Return
      End

C   APLICACION DEL METODO DE Euler modificado:

      Subroutine EULER(X,Y,Xn,H,Nprint)

      Real Y1
      Np=0
      Do While (X .LT. Xn)

C   CALCULO E IMPRESION DE LOS NUEVOS VALORES DE X y Y

          Y1=Y+H*F(X,Y)
          Y=Y+(H/2)*(F(X,Y)+F(X+H,Y1))
          X=X+H
          Np=Np+1
          If (Np .EQ. Nprint) Then
            Write(*,1)X,Y
            Np=0
          End If
      End Do
   1  Format(2F20.4)
      End

