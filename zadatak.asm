data segment
; Definicija podataka
    poruka1 db "Unesi broj u osnovi 7: $"
    poruka4 db "Prosti cinioci ovog broja: $"
    izlazPoruka db "Klikni bilo koji taster $"
    broj1 dw 0
    strBroj1 db "      "
    broj2 dw 0
    delioci dw ?
    strDelioci db "$$$$$$$"
    pocV dw 0
    polovinaD dw 0
ends
; Deficija stek segmenta
stek segment stack
    dw 128 dup(0)
ends
; Ucitavanje znaka bez prikaza i cuvanja     
keypress macro
    push ax
    mov ah, 08
    int 21h
    pop ax
endm
; Isis stringa na ekran
writeString macro s
    push ax
    push dx  
    mov dx, offset s
    mov ah, 09
    int 21h
    pop dx
    pop ax
endm
; Kraj programa           
krajPrograma macro
    mov ax, 4c02h
    int 21h
endm   
           
code segment
; Novi red
novired proc
    push ax
    push bx
    push cx
    push dx
    mov ah,03
    mov bh,0
    int 10h
    inc dh
    mov dl,0
    mov ah,02
    int 10h
    pop dx
    pop cx
    pop bx
    pop ax
    ret
novired endp
; Ucitavanje stringa sa tastature
; Adresa stringa je parametar na steku
readString proc
    push ax
    push bx
    push cx
    push dx
    push si
    mov bp, sp
    mov dx, [bp+12]
    mov bx, dx
    mov ax, [bp+14]
    mov byte [bx] ,al
    mov ah, 0Ah
    int 21h
    mov si, dx     
    mov cl, [si+1] 
    mov ch, 0
kopiraj:
    mov al, [si+2]
    mov [si], al
    inc si
    loop kopiraj     
    mov [si], '$'
    pop si  
    pop dx
    pop cx
    pop bx
    pop ax
    ret 4
readString endp
; Konvertuje string u broj
;ista metoda kao strtoint sa casa samo je osnova drugacija
iz7u10 proc
    push ax
    push bx
    push cx
    push dx
    push si
    mov bp, sp
    mov bx, [bp+14]
    mov ax, 0
    mov cx, 0
    mov si, 7 ; osnova
petlja1:
    mov cl, [bx]
    cmp cl, '$'
    je kraj1
    mul si
    sub cx, 48
    add ax, cx
    inc bx  
    jmp petlja1
kraj1:
    mov bx, [bp+12] 
    mov [bx], ax 
    pop si  
    pop dx
    pop cx
    pop bx
    pop ax
    ret 4
iz7u10 endp
; Konvertuje broj u string (osnova 7)

inttostr proc
   push ax
   push bx
   push cx
   push dx
   push si
   mov bp, sp
   mov ax, [bp+14] 
   mov dl, '$'
   push dx
   mov si, 10
petlja2c:
   mov dx, 0
   div si
   add dx, 48
   push dx
   cmp ax, 0
   jne petlja2c
   
   mov bx, [bp+12]
petlja2ac:      
   pop dx
   mov [bx], dl
   inc bx
   cmp dl, '$'
   jne petlja2ac
   pop si  
   pop dx
   pop cx
   pop bx
   pop ax 
   ret 4
inttostr endp

;ispisivanje broja sa osnovom 7
iz10u7 proc 
  call dollars ;ubacivanje dolara u string.
  mov  bx, 7  ;delimo sa 7 da bi dobili broj u toj osnovi.
  mov  cx, 0  ;brojac za cifre koje smo izvukli iz broja  
cycle1:       
  mov  dx, 0 
  div  bx;delimo sa osnovom 7,ax/7, dx ostatak     
  push dx;stavljamo dx na stek     
  inc  cx;inkrementiramo cx za svaku cifru koju izvlacimo iz stringa      
  cmp  ax, 0;ako broj nije 0 lupujemo 
  jne  cycle1  

cycle2:  
  pop  dx        
  add  dl, 48;konvertujemo cifru u karakter 
  mov  [ si ], dl
  inc  si;si sluzi kao pokazivac na memorijsku lokaciju gde da stavi karakter
  loop cycle2  

  ret
iz10u7 endp       

;ubacujemo dolare u string da bi ocistili string pre konvertovanja sledeceg broja
proc dollars                 
  mov  cx, 5
  mov  di, offset strDelioci
dollars_loop:      
  mov  bl, '$'
  mov  [ di ], bl
  inc  di
  loop dollars_loop

  ret
endp
;proverava da li je broj prost
proc jeProst
    push ax;stavljamo vrednosti registara na stek
    push bx
    push dx
    cmp delioci,2;2 je prost i ispisuje se
    je daljec
    mov dx,0;ubacujemo 0 u dx kako bi izbegli division overflow error
    mov ax,delioci
    mov bx,2
    div bx;delimo broj sa 2
    mov polovinaD,ax
    cmp dx,0;ako je broj deljiv sa 2 to znaci da nije prost
    je krj
    mov bx,3;krecemo od 3 do polovine broja
    mov pocV,bx;ubacujemo 3 u pocV
    l1: mov cx,polovinaD;u cx ubacujemo vrednost iz polovinaD
        cmp pocV,cx;gledamo da li smo presli polovinu
        jg daljec;ako jesmo idemo na labelu dalje,ako nismo 
        mov dx,0
        mov ax,delioci
        mov bx,pocV
        div bx
        cmp dx,0;ako je broj deljiv sa bilo kojim brojem onda on nije prost
        je krj;ako nije prost idemo odmah na kraj
        inc pocV; pocV++
        jmp l1
    daljec:
        mov  si, offset strDelioci;pocetak stringa
        mov  ax, delioci;u ax stavljamo broj koji smo proveravali
        call iz10u7    ;ispisuje broj sa osnovom 7
        writeString strDelioci;ispisujemo broj sa osnovom 7
        call novired
        jmp krj;idemo na kraj
        
    
    krj:
        pop dx
        pop bx
        pop ax;vracamo vrednosti registara sa steka
        ret
        ;ako smo presli polovinu i ako nismo nasli cinioca naseg broja,broj je prost
    endp





start:
    ; postavljanje segmentnih registara       
    ASSUME cs: code, ss:stek
    mov ax, data
    mov ds, ax
	
    ; Mesto za kod studenata
    call novired
    writestring poruka1;ispisujemo prompt za unos 
    push 6;max duzina stringa kojeg cemo uneti
    push offset strBroj1;stavljamo na stek pocetak stringa
    call readString; citamo string koji cemo posle konvertovati u broj
    
    push offset strBroj1
    push offset broj1
    call iz7u10;konvertujemo broj iz osnove 7 u osnovu 10
    mov dx,0;stavljamo 0 u dx da bi izbegli divide overflow
    mov ax,broj1;
    mov bx,2
    div bx;trazimo cinioce do polovine prosledjenog broja, ax = ax/bx
    mov broj2,ax;stavljamo izracunatu vrednost u promenljivu 
    call novired
    writeString poruka4;ispisivanje poruke pred pocetak ispisivanja prostih cinioca
    call novired
    mov delioci,1;pocinjemo od 1 i idemo do vrednosti koja je u broju 2
    pocetak:
        mov ax,broj2;u ax stavljamo vrednost iz broj2
        CMP delioci, ax       ;proveravamo sve brojeve <= od polovine unetog broja,tako sto uporedjujemo vrednost iz promenljive delioci sa vrednoscu u registru ax
        JBE PRINT;jbe - jump short if below or equal, ide nam na labelu prin                    
        JMP kraj;na kraju idemo na labelu kraj

     PRINT:
         mov dx,0
         mov ax,broj1
         mov bx,delioci
         div bx
         cmp dx,0; proveravamo da li je trenutna vrednost deljiva sa unetim brojem
         jne dalje;ako nije necemo proveravati da li je prost vec samo inkrementiramo promenljivu delioci
         je ispisi;ako jeste proveravamo da li je prost 
         ispisi: 
            call jeProst; pozivamo proceduru koja proverava da li je broj prost     
            jmp dalje;idemo na labelu dalje koja ce na inkrementira vrednost u promenljivoj delioci i skacemo na labelu pocetak
         dalje:
            INC delioci              ;delioci++
            JMP pocetak

    kraj:
        writestring izlazPoruka;ispis poruke za izlaz 
        keypress;ceka se da korisnik programa pritisne taster na tastaturi da bi zavrsio program 
        krajPrograma;makro za zavrsavanje programa 
ends
end start