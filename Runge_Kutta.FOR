C   Utec Tulancingo
C   Matematicas para ingenieria II
C   RESUELVE UN P.V.I. CON UNA ECUACION DIFERENCIAL ORDINARIA DE
C   PRIMER ORDEN POR EL METODO DE EULER

C   DESCRIPCION DE VARIABLES :

C   X     : VALOR INICIAL DE LA VARIABLE INDEPENDIENTE
C   XN    : VALOR FINAL DE LA VARIABLE DEPENDIENTE
C   H     : PASO DE INTEGRACION
C   Y     : VALOR DE LA VARIABLE INDEPENDIENTE EN X
C   NPRINT: CONTROL DE IMPRESION
C           (SE IMPRIMEN X y Y CADA NPRINT ITERACIONES)

      Program R_K_4
      Call Lee_Datos(X,Xn,H,Y,Nprint)
      Call Escribe_Datos(X,XN,H,Y,Nprint)
      Call Runge_Kutta_4(X,Y,Xn,H,Nprint)
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
      Write(*,*)'   Control de impresi¢n NPRINT                   '
      Read(*,*)Nprint


      
      End

C   Escritura de valores iniciales

      Subroutine Escribe_Datos(X,XN,H,Y,Nprint)

      Write(*,*)'   CONDICION INICIAL :     Y(',X,')= ',Y
      Write(*,*)'   PASO DE INTEGRACION H         = ',H
      Write(*,*)'   VALOR FINAL DE X              = ',XN
      Write(*,*)'   SE ESCRIBE CADA ',NPRINT,' ITERACIONES'
      Write(*,*)'         X             Y     '
      Write(*,*)'    __________    __________'
      Return
      End

C   APLICACION DE RUNGE KUTTA 4o ORDEN:

      Subroutine Runge_Kutta_4(X,Y,Xn,H,Nprint)
      Real K1,K2,K3,K4

      Np=0
      Do While (X .LT. Xn)
      
C     CALCULO DE LAS CONSTANTES K1, K2, K3 Y K4
          K1=F(X,Y)
          K2=F(X+H/2,Y+H*K1/2)
          K3=F(X+H/2,Y+H*K2/2)
          K4=F(X+H,Y+H*K3)
      
      
C   CALCULO E IMPRESION DE LOS NUEVOS VALORES DE X y Y

          Y=Y+H/6*(K1+2*K2+2*K3+K4)
          X=X+H
          Np=Np+1
          If (Np .EQ. Nprint) Then
            Write(*,1)X,Y
            Np=0
          End If
      End Do
   1  Format(2F14.4)
      End

 
