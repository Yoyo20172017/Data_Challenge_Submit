--use R to read csv first, clean the data and write into another csv file
--bank=read.csv("C:/Users/MU/Desktop/Apple Data Challenge/Data/bank-additional/bank-additional/bank-additional-full.csv",sep=";",header=TRUE)
--write.csv(bank,"C:/Users/MU/Desktop/Apple Data Challenge/Data/bank-additional-full.csv")

CREATE TABLE [Bank].[dbo].[bank-additional-full] (
 ID varchar(10),
 age	int,
 job	varchar(50),
 marital	varchar(50),
 education	varchar(50),
 default_	varchar(10),
 housing	varchar(50),
 loan	varchar(10),
 contact	varchar(50),
 month	varchar(10),
 day_of_week	varchar(50),
 duration	numeric(18, 0),
 campaign	numeric(18, 0),
 pdays	numeric(18, 0),
 previous	numeric(18, 0),
 poutcome	varchar(50),
 emp_var_rate	numeric(18, 0),
 cons_price_idx	numeric(18, 0),
 cons_conf_idx	numeric(18, 0),
 euribor3m	numeric(18, 0),
 nr_employed	numeric(18, 0),
 y	varchar(10)
	
);


BULK INSERT [Bank].[dbo].[bank-additional-full]
FROM 'C:/Users/MU/Desktop/Apple Data Challenge/Data/bank-additional-full.csv'
WITH
(
    FIRSTROW = 2,
    FIELDTERMINATOR = ',',  --CSV field delimiter
    ROWTERMINATOR = '\n',   --Use to shift the control to next row
    TABLOCK
)


SELECT * FROM [Bank].[dbo].[bank-additional-full]

UPDATE [Bank].[dbo].[bank-additional-full]
    SET ID = REPLACE(ID, '"', '') WHERE CHARINDEX('"', ID) <> 0
UPDATE [Bank].[dbo].[bank-additional-full]
	SET JOB = REPLACE(JOB, '"', '') WHERE CHARINDEX('"', JOB) <> 0
UPDATE [Bank].[dbo].[bank-additional-full]
	SET MARITAL = REPLACE(MARITAL, '"', '') WHERE CHARINDEX('"', MARITAL) <> 0
UPDATE [Bank].[dbo].[bank-additional-full]
	SET DEFAULT_ = REPLACE(DEFAULT_ , '"', '') WHERE CHARINDEX('"', DEFAULT_ ) <> 0
UPDATE [Bank].[dbo].[bank-additional-full]
	SET education = REPLACE(education, '"', '') WHERE CHARINDEX('"', education) <> 0
UPDATE [Bank].[dbo].[bank-additional-full]
	SET housing = REPLACE(housing, '"', '') WHERE CHARINDEX('"', housing) <> 0
UPDATE [Bank].[dbo].[bank-additional-full]
	SET loan = REPLACE(loan, '"', '') WHERE CHARINDEX('"', loan) <> 0
UPDATE [Bank].[dbo].[bank-additional-full]
	SET contact = REPLACE(contact, '"', '') WHERE CHARINDEX('"', contact) <> 0
UPDATE [Bank].[dbo].[bank-additional-full]
	SET month = REPLACE(month, '"', '') WHERE CHARINDEX('"', month) <> 0
UPDATE [Bank].[dbo].[bank-additional-full]
	SET day_of_week = REPLACE(day_of_week, '"', '') WHERE CHARINDEX('"', day_of_week) <> 0
UPDATE [Bank].[dbo].[bank-additional-full]
	SET poutcome = REPLACE(poutcome, '"', '') WHERE CHARINDEX('"', poutcome) <> 0
UPDATE [Bank].[dbo].[bank-additional-full]
	SET y = REPLACE(y, '"', '') WHERE CHARINDEX('"', y) <> 0