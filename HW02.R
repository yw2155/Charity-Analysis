rm(list=ls())
setwd('D:/ESSEC Study/Marketing Analysis/HW02')

library(RODBC)
library(nnet)

db = odbcConnect("mysql_server_64", uid = "root", pwd = "")
sqlQuery(db, "USE ma_charity_full")

query1 = "SELECT a.contact_id,
                DATEDIFF(20180701, MAX(a.act_date)) / 365 AS 'recency',
                COUNT(a.amount)/DATEDIFF(20180701,MIN(a.act_date)) AS 'frequency',
                AVG(a.amount) AS 'avgamount',
                MAX(a.amount) AS 'maxamount',
                IF(c.counter IS NULL, 0, 1) AS 'loyal',
                c.targetamount AS 'targetamount'
         FROM acts a
         LEFT JOIN (SELECT contact_id, COUNT(amount) AS counter, amount AS targetamount
                    FROM assignment2
                    WHERE calibration=1
                    GROUP BY contact_id) AS c
         ON c.contact_id = a.contact_id
         WHERE (act_type_id = 'DO') AND (act_date < 20180701)
         GROUP BY 1"

training_v1 = sqlQuery(db, query1)

test_query = "SELECT id AS contact_id, 
	                  CASE 
		                  WHEN prefix_id = 'MME' OR prefix_id = 'MLLE' THEN 4
    	                WHEN prefix_id = 'MR' THEN 2
    	                ELSE 1
                    END
	            AS gender FROM contacts"
test_gender = sqlQuery(db, test_query)
training_v2 = merge(x = training_v1, y = test_gender, by = "contact_id", all.x = TRUE)
training_v2$gender[1] = 1


#Calculate contact's probability of responding to campaign
test_query2 = "SELECT contact_id, COUNT(amount) AS 'respond' FROM acts WHERE campaign_id IS NOT NULL GROUP BY contact_id"
test_respond = sqlQuery(db, test_query2)
test_query3 = "SELECT contact_id, COUNT(campaign_id) AS 'Total' FROM actions GROUP BY contact_id"
test_total = sqlQuery(db, test_query3)
temper = merge(x = test_respond, y = test_total, by = 'contact_id', all.x = TRUE)
temper$Total[is.na(temper$Total)] = 1
temper$rate = temper$respond/temper$Total
temper$rate[temper$rate>=1] = 1
rate = subset(temper, select = c('contact_id','rate'))

training_v3 = merge(x = training_v2, y = rate, by = 'contact_id', all.x = TRUE)
training_v3$rate[is.na(training_v3$rate)] = 0.5

prob.model = multinom(formula = loyal ~ (recency*frequency)+log(recency)+log(frequency), data=training_v3)

z = which(!is.na(training_v3$targetamount))
amount.model = lm(formula = log(targetamount) ~ log(avgamount) + log(maxamount), data = training_v3[z, ])

query2 = "SELECT contact_id FROM assignment2 WHERE calibration=0"
testing_base = sqlQuery(db, query2)

query3 = "SELECT contact_id,
                DATEDIFF(20180701, MAX(act_date))
                   / 365 AS 'recency',
                COUNT(amount)/DATEDIFF(20180701,MIN(act_date)) AS 'frequency',
                AVG(amount) AS 'avgamount',
                MAX(amount) AS 'maxamount'
         FROM acts
         WHERE (act_type_id = 'DO')
         GROUP BY 1"
testing_v1 = sqlQuery(db, query3)


odbcClose(db)

testing_t = merge(x = testing_base, y = testing_v1, by = "contact_id", all.x = TRUE)
testing_f = merge(x = testing_t, y = rate, by = "contact_id", all.x = TRUE)


#GAIN CHART
valid = predict(prob.model, training_v3, type = 'probs')
target = training_v3$loyal[order(valid, decreasing=TRUE)] / sum(training_v3$loyal)

gainchart = c(0, cumsum(target))

# Create a random selection sequence
random = seq(0, to = 1, length.out = length(training_v3$loyal))

# Create the "perfect" selection sequence
perfect = training_v3$loyal[order(training_v3$loyal, decreasing=TRUE)] / sum(training_v3$loyal)
perfect = c(0, cumsum(perfect))

q = c(0.01, 0.05, 0.10, 0.25, 0.50)
x = quantile(gainchart, probs = q)
z = quantile(perfect,   probs = q)

print("Hit rate:")
print(x)

print("Lift:")
print(x/q)

print("Improvement:")
print((x-q)/(z-q))


#PREDICTION
probs = predict(prob.model, testing_f, type='probs')
amount = exp(predict(amount.model, testing_f))
score = probs * amount
for (i in 1:length(score)) {
  if(is.na(score[i])){score[i]=0}
}

pred = data.frame(contact_id = testing_f$contact_id, solicit = score)
pred$solicit[pred$solicit <= 2] = 0
pred$solicit[pred$solicit > 2] = 1

print(head(pred))
z = which(pred$solicit == 0)
print(length(z))


write.table(pred, file = "submit.txt", sep = "\t",
            row.names = FALSE, col.names = FALSE)

