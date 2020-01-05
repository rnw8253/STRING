! string_2d is a program to compute the minimum energy path on a 2d potential surface.  You must provide the starting points and the program then
! creates an initial path as straight line between these two points.
! USAGE: string_2d.x < [data file]

!modules

module inputdata

        ! Number of bins
        integer, save :: nXvals = 100
        integer, save :: nYvals = 100

        ! start and stop position in number of bins (might want to change this)
        integer, save :: startPath(2) = (/ 71,23 /)
        integer, save :: stopPath(2) = (/ 16,71 /)

        ! arrays to be read in 
        real (kind=8) , allocatable :: zVal(:,:)
        real (kind=8) , allocatable :: xVal(:)
        real (kind=8) , allocatable :: yVal(:)

endmodule inputdata


! MAIN

program find_minimal_1d_path

        call read_data()

        call string_path()
        
endprogram find_minimal_1d_path

! Subroutines


! read the 2d PE data
subroutine read_data
        use inputdata
        implicit none
        integer x, y
        real (kind=8) minZ, maxZ

        allocate(xVal(nXvals),yVal(nYvals),zVal(nXvals,nYvals))

        do x=1,nXvals
                do y=1,nYvals
                        read(*,'(3f20.10)') xVal(x),yVal(y),zVal(x,y)
                        zVal(x,y) = zVal(x,y)*627.5095
                        if ((y==1 .and. x==1) .or. zVal(x,y)<minZ) then
                                minZ = zVal(x,y)
                        endif
                enddo
        enddo
        !Zero everything
        zVal = zVal-minZ
               

endsubroutine read_data


!Comput ethe string
subroutine string_path
        use inputdata
        implicit none
        integer, parameter :: maxIt = 50 ! maximum number of iterations
        integer, parameter :: nImages = 24 ! how many points to put along string
        integer, parameter :: searchSize = 4 ! the radius in number of bins in my search space.  The bigger this value the longer it takes but less likely you will get stuck in local minima
        character*60 outputFile
        logical converged
        logical new
        integer x, y
        integer currentX, currentY
        integer tempX, tempY
        integer minX, minY
        integer maxX, maxY
        integer counter
        integer iter
        real (kind=8) minZ
        integer image
        integer image2
        integer xCoord(nImages)
        integer yCoord(nImages)
        integer xCoordTemp(nImages)
        integer yCoordTemp(nImages)
        real (kind=8) imageDist(nImages)
        real (kind=8) xDist
        real (kind=8) yDist
        real (kind=8) distance
        real (kind=8) distTemp
        real (kind=8) delta
        character num1*1,num2*2,num3*3

        ! determine intial path of string
        yDist = dble(stopPath(2)-startPath(2))
        xDist = dble(stopPath(1)-startPath(1))
        distance = abs(xDist)+abs(yDist)
        delta = distance/dble(nImages+1)
        outputFile = "concerted_string000.dat"
        open(10,file=outputFile)
        write(10,'(2i6,f20.10)') startPath(1), startPath(2), zVal(startPath(1),startPath(2))
        do image = 1, nImages
!                if (image==1 .or. xCoord(image-1) > stopPath(1)) then
!                        xCoord(image) = startPath(1) - int(image*delta)
!                        yCoord(image) = startPath(2) 
!                else
!                        xCoord(image) = stopPath(1)
!                        yCoord(image) = yCoord(image-1) + int(delta)
!                endif
                xCoord(image) = startPath(1) + int(image*delta*xDist/distance)
                yCoord(image) = startPath(2) + int(image*delta*yDist/distance)
                write(10,'(2i6,f20.10)') xCoord(image),yCoord(image), zVal(xCoord(image),yCoord(image))
        enddo
        write(10,'(2i6,f20.10)') stopPath(1), stopPath(2), zVal(stopPath(1),stopPath(2))
        close(10)
        print*, "Delta for iter", 0, "is:", delta

        ! iterate the string
        iter = 1 
        converged = .false.
        do while (iter < maxIt) 
                do image = 1, nImages
                       currentX = xCoord(image)
                       currentY = yCoord(image)
                       if (currentX - searchSize <= 0) then
                               minX = 1
                               maxX = currentX + searchSize
                       elseif (currentX + searchSize > nXvals) then
                               minX = nXvals - searchSize
                               maxX = nXvals
                       else
                               minX = currentX - searchSize
                               maxX = currentX + searchSize
                       endif
                       if (currentY - searchSize <= 0) then
                               minY = 1
                               maxY = currentY + searchSize
                       elseif (currentY + searchSize > nYvals) then
                               minY = nYvals - searchSize
                               maxY = nYvals
                       else
                               minY = currentY - searchSize
                               maxY = currentY + searchSize
                       endif
        
                       counter = 0
                       do x=minX, maxX
                               do y=minY, maxY
                                       if (x==currentX .and. y==currentY) then
                                       else
                                              if (counter ==0 .or. zVal(x,y) < minZ) then
                                                     minZ = zVal(x,y)
                                                     tempX = x
                                                     tempY = y
                                              endif 
                                              counter = counter + 1
                                       endif
                               enddo
                       enddo
        
                       xCoord(image) = tempX
                       yCoord(image) = tempY
                enddo


                ! Now to readjust the string
                ! First compute new spacing
                yDist = dble(startPath(2)-yCoord(1))
                xDist = dble(startPath(1)-xCoord(1))
                distance = sqrt(xDist*xDist+yDist*yDist)
                do image=1,nImages-1
                        yDist = dble(yCoord(image+1)-yCoord(image))
                        xDist = dble(xCoord(image+1)-xCoord(image))
                        distance = distance + sqrt(xDist*xDist+yDist*yDist)
                enddo
                yDist = dble(stopPath(2) - yCoord(nImages))
                xDist = dble(stopPath(1) - xCoord(nImages))
                distance = distance + sqrt(xDist*xDist+yDist*yDist)
                delta = distance/dble(nImages+1)
                print*, "Delta for iter", iter, "is:", delta
                ! Place new images evenly along string
                do image = 1, nImages
                        distance = 0
                        do image2=1,nImages
                                if (image2==1) then
                                        yDist = dble(yCoord(1) - startPath(2))
                                        xDist = dble(xCoord(1) - startPath(1))
                                        distTemp = sqrt(xDist*xDist+yDist*yDist)
                                        imageDist(image2) = distTemp
                                else
                                        yDist = dble(yCoord(image2)-yCoord(image2-1))
                                        xDist = dble(xCoord(image2)-xCoord(image2-1))
                                        distTemp = sqrt(xDist*xDist+yDist*yDist)
                                        imageDist(image2) = imageDist(image2-1) + distTemp
                                endif
                                distance = distance + distTemp
                                if (distance > image*delta) exit
                        enddo
!                        print*, "Image ",image, "goes between previous images", image2-1, "and", image2
                        if (image2==1) then
                                xCoordTemp(image) = startPath(1) + int(image*delta*xDist/distTemp)
                                yCoordTemp(image) = startPath(2) + int(image*delta*yDist/distTemp)
                        else
                                xCoordTemp(image) = xCoord(image2-1) + int((image*delta-imageDist(image2-1))*xDist/distTemp)
                                yCoordTemp(image) = yCoord(image2-1) + int((image*delta-imageDist(image2-1))*yDist/distTemp)
                        endif
                enddo

                xCoord = xCoordTemp
                yCoord = yCoordTemp
                
                iter = iter + 1
                if (iter > maxIt) exit

        enddo

                if (maxIt<10) then
                        write(num1,'(i1)') maxIt
                        outputFile="concerted_string00"//num1//".dat"
                elseif (maxIt<100) then
                        write(num2,'(i2)') maxIt
                        outputFile="concerted_string0"//num2//".dat"
                elseif (maxIt<1000) then
                        write(num3,'(i3)') maxIt
                        outputFile="concerted_string"//num3//".dat"
                else
                        print*,"Iterations have gotten too large.  Bombing out"
                        stop
                endif
                open(10,file=outputFile,position="rewind")
!                write(10,'(3f10.5,f20.10)') 0.0, xVal(startPath(1)), yVal(startPath(2)),zVal(startPath(1),startPath(2))
                write(10,'(f10.5,2i10,f20.10)') 0.0, startPath(1), startPath(2),zVal(startPath(1),startPath(2))
                do image = 1, nImages
!                        write(10,'(3f10.5,f20.10)') real(image/real(nImages+1)),xVal(xCoord(image)),yVal(yCoord(image)), zVal(xCoord(image),yCoord(image))
                        write(10,'(f10.5,2i10,f20.10)') real(image/real(nImages+1)),xCoord(image),yCoord(image), zVal(xCoord(image),yCoord(image))
                enddo
!                write(10,'(3f10.5,f20.10)') 1.0, xVal(stopPath(1)), yVal(stopPath(2)),zVal(stopPath(1),stopPath(2))
                write(10,'(f10.5,2i10,f20.10)') 1.0, stopPath(1), stopPath(2),zVal(stopPath(1),stopPath(2))
                close(10)

endsubroutine string_path

