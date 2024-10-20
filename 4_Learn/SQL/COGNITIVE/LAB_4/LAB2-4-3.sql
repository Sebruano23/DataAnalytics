-- EX 1 --
SELECT DEP_ID, COUNT(EMP_ID) AS EMP_COUNT
FROM EMPLOYEES
GROUP BY DEP_ID 
ORDER BY EMP_COUNT DESC;

-- MI VERSION --

SELECT d.DEP_NAME, COUNT(e.EMP_ID) AS EMP_COUNT
FROM EMPLOYEES AS e 
JOIN DEPARTMENTS AS d 
ON e.DEP_ID = d.DEPT_ID_DEP
GROUP BY d.DEP_NAME 
ORDER BY EMP_COUNT DESC;


-- EX 2 y 3 --

SELECT d.DEP_NAME, COUNT(e.EMP_ID) AS No_EMPLOYEES, AVG(e.SALARY) AS AVG_SALARY
FROM EMPLOYEES AS e
JOIN DEPARTMENTS AS d 
ON e.DEP_ID = d.DEPT_ID_DEP
GROUP BY d.DEP_NAME
ORDER BY d.DEP_NAME;

-- EX 4 --
SELECT d.DEP_NAME, COUNT(e.EMP_ID) AS No_EMPLOYEES, AVG(e.SALARY) AS AVG_SALARY
FROM EMPLOYEES AS e
JOIN DEPARTMENTS AS d 
ON e.DEP_ID = d.DEPT_ID_DEP
GROUP BY d.DEP_NAME
ORDER BY AVG_SALARY;

-- EX 5 --
SELECT d.DEP_NAME, COUNT(e.EMP_ID) AS No_EMPLOYEES, AVG(e.SALARY) AS AVG_SALARY
FROM EMPLOYEES AS e
JOIN DEPARTMENTS AS d 
ON e.DEP_ID = d.DEPT_ID_DEP
GROUP BY d.DEP_NAME
HAVING COUNT(e.EMP_ID) < 4
ORDER BY AVG_SALARY;
