;PLAYER ATTRIBUTES
PLAYER_X = $04
PLAYER_Y = $05
PLAYER_DIR = $06
PLAYER_SPRITE = $07
PLAYER_LIVES = $08
PLAYER_ALIVE = $09
PLAYER_ADDR_LO = $0A
PLAYER_ADDR_HI = $0B
; PLAYER_COLOUR_LO = $0C
; PLAYER_COLOUR_HI = $0D

;OBJECTS
	SEG.U enemies
	ORG $0C
num_enemies ds 1
enemy_sprite ds 8
enemy_dir ds 8
enemy_x ds 8
enemy_y ds 8
enemy_low ds 8
enemy_high ds 8
enemy_alive ds 8

	SEG.U bullets
	ORG $45
numbullet ds 1
bullet_sprite ds 8
bullet_dir ds 8
bullet_x ds 8
bullet_y ds 8
bullet_low ds 8
bullet_high ds 8
; bullet_colour ds 8			;To delete if needed
bullet_collide ds 8
	SEG

CUR_LEVEL = $76
CUR_SPRITE = $77
OBJECT_DIR = $78
COUNTER = $79
COLLISION_STATUS = $7A
SCRATCH_LO = $7B
SCRATCH_HI = $7C
REMAINDER = $7D
ENEMY_SHOOT_TIMER = $7E
ENEMY_MOVE_TIMER = $7F
ENEMY_CYCLE_CTR = $80
ENEMY_DIFF_X = $81
ENEMY_DIFF_Y = $82
ENEMY_COUNTER = $83
COLL_PTR_LO = $84
COLL_PTR_HI = $85
LEVEL_CHANGE = $86
LEVEL_OFFSET = $87
LEVEL_ADDR_LO = $88
LEVEL_ADDR_HI = $89
LEVEL_LOWERADDR_LO = $8A
LEVEL_LOWERADDR_HI = $8B
TEMP_BYTE = $8C