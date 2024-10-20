-- EX 1 --
SELECT F_NAME, L_NAME, DEP_ID
FROM EMPLOYEES
ORDER BY DEP_ID;

-- EX 2 --

SELECT F_NAME, L_NAME, DEP_ID 
FROM EMPLOYEES 
ORDER BY DEP_ID DESC, L_NAME DESC;


-- EX 3 --

SELECT e.F_NAME,e.L_NAME,d.DEP_NAME
FROM EMPLOYEES AS e 
JOIN DEPARTMENTS AS d 
ON e.DEP_ID = d.DEPT_ID_DEP
ORDER BY d.DEP_NAME, e.L_NAME DESC;

