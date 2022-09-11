use gamestore;

SET
    NAMES 'utf8mb4' COLLATE 'utf8mb4_bin';

INSERT INTO
    `logins` (`email`, `password`)
VALUES
    (
        'u1@wp.pl',
        '$2a$12$ibsZd73HtfcYdov3r9j9vev5mRJC4K3fArG1kGhCpVC4bzp.hgW.u'
    ),
    (
        'u2@wp.pl',
        '$2a$12$ibsZd73HtfcYdov3r9j9vev5mRJC4K3fArG1kGhCpVC4bzp.hgW.u'
    );


INSERT INTO
    `users` (`email`, `name`)
VALUES
    (
        'u1@wp.pl',
        'User no 1'
    ),
    (
        'u2@wp.pl',
        'User 2'
    );


INSERT INTO
    `items` (
        `name`,
        `desc`,
        `price`
    )
VALUES
    (
        'item1',
        'desc for item 1',
        '199'
    );
