-- EX 1 --
SELECT e.F_NAME, e.L_NAME, j.START_DATE
FROM EMPLOYEES e INNER JOIN JOB_HISTORY j
ON e.EMP_ID = j.EMPL_ID
WHERE e.DEP_ID = '5'
ORDER BY e.F_NAME;

SELECT e.F_NAME, e.L_NAME, j.START_DATE
FROM EMPLOYEES e 
INNER JOIN JOB_HISTORY j 
ON e.EMP_ID = j.EMPL_ID
INNER JOIN DEPARTMENTS d 
ON e.DEP_ID = d.DEPT_ID_DEP
WHERE d.DEP_NAME = 'Software Group'
ORDER BY e.F_NAME;

-- EX 2 --
SELECT e.F_NAME, e.L_NAME, jh.START_DATE, j.JOB_TITLE
FROM EMPLOYEES e 
INNER JOIN JOB_HISTORY jh
ON e.EMP_ID = jh.EMPL_ID
INNER JOIN JOBS j 
ON j.JOB_IDENT = jh.JOBS_ID
WHERE e.DEP_ID = '5';

-- EX 3 --

SELECT e.EMP_ID, e.L_NAME, e.DEP_ID, d.DEP_NAME
FROM EMPLOYEES AS e 
LEFT OUTER JOIN DEPARTMENTS AS d 
ON e.DEP_ID = d.DEPT_ID_DEP;

-- TODOS LOS EMPLEADOS TIENEN UN DEPARTAMENTO DEFINIDO --

-- EX 4 --

SELECT e.EMP_ID, e.L_NAME, e.DEP_ID, d.DEP_NAME
FROM EMPLOYEES AS e 
LEFT OUTER JOIN DEPARTMENTS AS d 
ON e.DEP_ID = d.DEPT_ID_DEP
WHERE YEAR(e.B_DATE) < 1980;

-- EX 5 --

SELECT e.EMP_ID, e.L_NAME, e.DEP_ID,d.DEP_NAME
FROM EMPLOYEES AS e 
LEFT OUTER JOIN DEPARTMENTS AS d 
ON e.DEP_ID = d.DEPT_ID_DEP
AND YEAR(e.B_DATE) < 1980;

-- EX 6 --

SELECT e.F_NAME, e.L_NAME, d.DEP_NAME
FROM EMPLOYEES AS e 
FULL OUTER JOIN DEPARTMENTS AS d 
ON e.DEP_ID = d.DEPT_ID_DEP;

-- EX 7 --

SELECT e.F_NAME, e.L_NAME,d.DEPT_ID_DEP,d.DEP_NAME
FROM EMPLOYEES AS e 
FULL OUTER JOIN DEPARTMENTS AS d 
ON e.DEP_ID = d.DEPT_ID_DEP 
AND e.SEX = 'M';