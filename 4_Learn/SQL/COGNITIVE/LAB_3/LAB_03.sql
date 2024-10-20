-- CREAR TABLA INSTRUCTOR LABORATORIO 3 --

-- PASO 0--

DROP table INSTRUCTOR;

-- PASO 1 --

CREATE TABLE INSTRUCTOR 

	( 
	
	ins_id INTEGER PRIMARY KEY NOT NULL,
	lastname VARCHAR(20) NOT NULL,
	firstname VARCHAR(20) NOT NULL, 
	city VARCHAR(10), 
	country CHAR(2)

	);

-- PASO 2A --

INSERT into INSTRUCTOR
	(ins_id,lastname,firstname,city,country)
VALUES 
	('1','Ahuja','Rav','Toronto','CA');
	
-- PASO 2B --

INSERT into INSTRUCTOR 
 (ins_id,lastname,firstname,city,country)
VALUES 
	('2','Chong','Raul','Toronto','CA'),
	('3','Vasudevan','Hima','Chicago','US');

-- PASO 3 --

SELECT * FROM INSTRUCTOR;

-- PASO 3A --

SELECT firstname,lastname from INSTRUCTOR WHERE city='Toronto';

-- PASO 4 --
UPDATE INSTRUCTOR SET City='Markham' WHERE ins_id='1';

-- PASO 5 --
DELETE FROM INSTRUCTOR WHERE ins_id='2';
SELECT * FROM INSTRUCTOR;


