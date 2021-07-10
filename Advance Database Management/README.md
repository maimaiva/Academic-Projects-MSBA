### Advance Database Management 

In class SQL Challange

```
/* Question 3*/
SELECT Customer_name 
FROM Customer_t 
WHERE Customer_ID IN 
	(
	SELECT Customer_ID 
	FROM Order_t 
	WHERE Order_ID IN
		(
		SELECT Order_ID 
		FROM Order_line_t 
		WHERE Product_ID IN
			(
			SELECT Product_ID 
			FROM Product_t 
			WHERE Product_Name = 'Computer Desk'
			)
		)
	)
```


EXCEPT statement 
```
/*Question 4*/
SELECT C.Customer_Name
FROM Customer_t as C INNER JOIN Order_t as O on C.Customer_ID = O.Customer_ID
WHERE NOT EXISTS
	(
		(
		SELECT P.Product_ID
		FROM Product_t as P
		WHERE Product_Name IN ('End Table', 'Coffee Table')
		)
	EXCEPT
		(
		SELECT OL.Product_ID
		FROM Order_Line_t as OL
		WHERE OL.Order_ID = O.Order_ID
		)
	)
```

Query the customers who had the highest total purchase using information from joining 3 Tables
```
/*Question 5*/
SELECT Top 1 Customer_Name, SUM(Quantity*P.Unit_Price) as Total_Purchases
FROM Order_t as O, Order_Line_t as OL,  Customer_t as C, Product_t as P
WHERE C.Customer_ID = O.Customer_ID AND O.Order_ID = OL.Order_ID AND OL.Product_ID = P.Product_ID AND
O.Order_Date BETWEEN '1/1/2011' and '12/31/2011'
GROUP BY Customer_Name
ORDER BY Total_Purchases Desc;
```
```
/*Question 6*/
SELECT R.Material_ID, R.Material_Description, V.Vendor_ID, V.Vendor_name, S.Unit_Price
FROM (Vendor_t as V inner join Supplies_t as S ON V.Vendor_ID = S.Vendor_ID) 
INNER JOIN Raw_Materials_t as R ON S.Material_ID = R.Material_ID
WHERE S.Unit_Price IN
	(
	Select Top 2 S2.Unit_Price
	From Supplies_t as S2
	Where S2.Material_ID = R.Material_ID
	Order By S2.Unit_Price
	)
Order By R.Material_ID, S.Unit_Price
```

```
/*Question 7*/	
SELECT PH.PatientID, P.Name, PH.HccCode
FROM Patient as P
JOIN PatientHcc as PH
ON P.ID = PH.PatientID
EXCEPT
    (
    SELECT PH.PatientID, P.Name, BR.HccCode
    FROM Patient as P
    JOIN PatientHcc as PH
    ON P.ID = PH.PatientID
    JOIN BizRule as BR
    ON PH.HccCode = BR.MainHccCode
    )
```
