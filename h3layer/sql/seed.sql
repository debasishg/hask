-- seed SQL file used for testing purposes

-- inserts manager with the name 'admin' and hash of the password '123'
INSERT INTO users (id, email, name, pwd_hash)
VALUES ('id1', 'test@test.com', 'User Userov', '$2y$14$dkHVYLbmnTTeT8DUuAsM5uOz7djmrcuzUv5jXurxveUk0vRca/AqW');

INSERT INTO accounts (no, name, user_id)
VALUES ('acc1', 'acc-name-1', 'id1');

INSERT INTO transactions (id, amount, account_no)
VALUES ('id1', 100.00, 'acc1');

INSERT INTO transactions (id, amount, account_no)
VALUES ('id2', 200.00, 'acc1');

INSERT INTO transactions (id, amount, account_no)
VALUES ('id3', 300.00, 'acc1');